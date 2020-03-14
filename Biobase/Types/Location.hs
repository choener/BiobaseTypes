
-- | Annotate the genomic @Location@ of features or elements. A @Location@ is
-- always contiguous, using strand, 0-based position, and length.
-- Transformation to different systems of annotation is made possible.

module Biobase.Types.Location where

import Control.DeepSeq
import Control.Lens hiding (Index, index)
import Data.Coerce
import Data.Data
import Data.Data.Lens
import GHC.Generics (Generic)
import GHC.TypeNats
import Prelude hiding (length)
import qualified Data.ByteString as BS
import qualified Streaming.Internal as SI
import qualified Streaming.Prelude as SP
import Text.Printf

import Biobase.Types.BioSequence
import Biobase.Types.Index
import Biobase.Types.Position
import Biobase.Types.Strand
import Data.Info




-- | Operations on locations.

class ModifyLocation posTy seqTy where
  -- | Append to the left.
  locAppendLeft  :: seqTy -> Location i posTy seqTy -> Location i posTy seqTy
  -- | Append to the right.
  locAppendRight :: seqTy -> Location i posTy seqTy -> Location i posTy seqTy
  -- | Split a location.
  locSplitAt  :: Int -> Location i posTy seqTy -> (Location i posTy seqTy, Location i posTy seqTy)
  -- | Length of location
  locLength :: Location i posTy seqTy -> Int

locTake k = fst . locSplitAt k

locTakeEnd k loc = let l = locLength loc in snd $ locSplitAt (l-k) loc

locDrop k = snd . locSplitAt k

locDropEnd k loc = let l = locLength loc in fst $ locSplitAt (l-k) loc

locSplitEndAt k loc = let l = locLength loc in locSplitAt (l-k) loc



data Location ident posTy seqTy = Location
  { _locIdentifier  :: !(SequenceIdentifier ident)
  , _locPosition    :: !posTy
  , _locSequence    :: !seqTy
  }
  deriving stock (Show,Data,Typeable,Generic)
makeLenses ''Location

instance (NFData p, NFData s) => NFData (Location i p s)

retagLocation :: Location i posTy seqTy -> Location j posTy seqTy
{-# Inline retagLocation #-}
retagLocation = over locIdentifier coerce

instance ModifyLocation FwdPosition (BioSequence w) where
  {-# Inline locAppendLeft #-}
  locAppendLeft s loc = let l = s^._BioSequence.to BS.length
    in loc & locSequence %~ (s <>) & locPosition %~ (\p -> if p^.fwdStrand == PlusStrand then p & fwdStart %~ (-. l) else p)
  {-# Inline locAppendRight #-}
  locAppendRight s loc = let l = s^._BioSequence.to BS.length
    in loc & locSequence %~ (<> s) & locPosition %~ (\p -> if p^.fwdStrand == MinusStrand then p & fwdStart %~ (-. l) else p)
  {-# Inline locSplitAt #-}
  locSplitAt k loc =
    let (h',t') = loc^.locSequence._BioSequence.to (BS.splitAt k)
        hl = BS.length h' ; tl = BS.length t'
        h = loc & locSequence._BioSequence .~ h' & locPosition %~ (\p -> if p^.fwdStrand == MinusStrand then p & fwdStart %~ (+. tl) else p)
        t = loc & locSequence._BioSequence .~ t' & locPosition %~ (\p -> if p^.fwdStrand == PlusStrand then p & fwdStart %~ (+. hl) else p)
    in  (h,t)
  {-# Inline locLength #-}
  locLength = view (locSequence._BioSequence.to BS.length)

instance ModifyLocation FwdPosition Int where
  {-# Inline locAppendLeft #-}
  locAppendLeft k' loc = let k = max 0 $ min (loc^.locPosition.fwdStart.to toInt0) k' in
    loc & locSequence %~ (+ k) & locPosition %~ (\p -> if p^.fwdStrand == PlusStrand then p & fwdStart %~ (-. k) else p)
  {-# Inline locAppendRight #-}
  locAppendRight k' loc = let k = max 0 $ min (loc^.locPosition.fwdStart.to toInt0) k' in
    loc & locSequence %~ (+ k) & locPosition %~ (\p -> if p^.fwdStrand == MinusStrand then p & fwdStart %~ (-. k) else p)
  {-# Inline locSplitAt #-}
  locSplitAt k loc =
    let h' = max 0 . min k $ locLength loc
        t' = locLength loc - h'
        h = loc & locSequence .~ h' & locPosition %~ (\p -> if p^.fwdStrand == MinusStrand then p & fwdStart %~(+. t') else p)
        t = loc & locSequence .~ t' & locPosition %~ (\p -> if p^.fwdStrand == PlusStrand then p & fwdStart %~ (+. h') else p)
    in  (h,t)
  {-# Inline locLength #-}
  locLength = view locSequence

instance Reversing (Location i FwdPosition (BioSequence w)) where
  {-# Inline reversing #-}
  reversing = over (locSequence._BioSequence) BS.reverse . over (locPosition) reversing

instance Complement (BioSequence w) => Complement (Location i FwdPosition (BioSequence w)) where
  {-# Inline complement #-}
  complement = iso f f
    where f = over locSequence (view complement)

instance (Info (BioSequence w)) => Info (Location i FwdPosition (BioSequence w)) where
  info loc = printf "%s %s %s" (loc^.locIdentifier^.to show) (show $ loc^.locPosition) (loc^.locSequence.to info)

-- | Will extract a substring for a given biosequence. It is allowed to hand in partially or not at
-- all overlapping locational information. This will yield empty resulting locations.
--
-- This will convert the @FwdPosition@ strand, which in turn allows dealing with reverse-complement
-- searches.
--
-- @
-- 0123456789
--    3.3
-- @

subLocation :: Location i FwdPosition (BioSequence w) -> (FwdPosition, Int) -> Location i FwdPosition (BioSequence w)
{-# Inline subLocation #-}
subLocation s (p',l)
  | ss==PlusStrand = locTake l $ locDrop d s
  | ss==MinusStrand = locTakeEnd l $ locDropEnd d s
  where ss = s^.locPosition.fwdStrand
        p = if ss == p'^.fwdStrand then p' else reversing p'
        d = delta (s^.locPosition.fwdStart) (p^.fwdStart)

data PIS i p s = PIS
  { _pisPrefix  :: Maybe (Location i p s)
  , _pisInfix   :: !(Location i p s)
  , _pisSuffix  :: Maybe (Location i p s)
  }
  deriving stock (Show, Data)
makeLenses ''PIS

pis ifx = PIS Nothing ifx Nothing

retagPis :: PIS i p s -> PIS j p s
retagPis (PIS p i s) = PIS (fmap retagLocation p) (retagLocation i) (fmap retagLocation s)

-- | Given a @PIS@, this will return the @substring@ indicated by the location in the 2nd argument.
-- Allows for easy substring extraction, and retains the system of prefix/infix/suffix.
--
-- It is allowed to hand locations that only partially (or not at all) correspond to the @PIS@, but
-- then the resulting @PIS@ will be empty!

subPisLocation :: PIS i FwdPosition (BioSequence w) -> (FwdPosition, Int) -> PIS i FwdPosition (BioSequence w)
{-# Inline subPisLocation #-}
subPisLocation pis loc =
  let f z = subLocation z loc
  in  over (pisPrefix._Just) f . over pisInfix f $ over (pisSuffix._Just) f pis

instance (Reversing (Location i FwdPosition (BioSequence w))) => Reversing (PIS i FwdPosition (BioSequence w)) where
  {-# Inline reversing #-}
  reversing pis
    = over (pisPrefix._Just) reversing . over pisInfix reversing . over (pisSuffix._Just) reversing
    . set pisPrefix (pis^.pisSuffix) . set pisSuffix (pis^.pisPrefix) $ pis

instance Complement (BioSequence w) => Complement (PIS i FwdPosition (BioSequence w)) where
  {-# Inline complement #-}
  complement =
    let f = over pisInfix (view complement) . over (pisPrefix._Just) (view complement) . over (pisSuffix._Just) (view complement)
    in  iso f f

pisSequence :: Lens (PIS i p (BioSequence s)) (PIS i p (BioSequence t)) (BioSequence s) (BioSequence t)
{-# Inline pisSequence #-}
pisSequence = lens f t where
  v = view (locSequence.bioSequence)
  f (PIS p i s) = BioSequence $ maybe BS.empty v p `BS.append` v i `BS.append` maybe BS.empty v s
  t (PIS p i s) (BioSequence str) =
    let (pfx,ifxsfx) = over _1 BioSequence   $ BS.splitAt (maybe 0 (BS.length . v) p) str
        (ifx,sfx   ) = over both BioSequence $ BS.splitAt (BS.length $ v i) ifxsfx
    in  PIS (set (_Just . locSequence) pfx p) (set locSequence ifx i) (set (_Just . locSequence) sfx s)



-- | Given a @Location@ with a @BioSequence@, replace the sequence with its length.

locAsLength :: Location i FwdPosition (BioSequence w) -> Location i FwdPosition Int
{-# Inline locAsLength #-}
locAsLength = over locSequence (view (_BioSequence.to BS.length))



-- | Provides a range in a notation as used by blast, for example. This
-- isomorphism can translate back as well. @FwdLocation - 8 4 ^. blastRange1 ==
-- 9 6 MinusStrand@, since these ranges are 1-based and start and end included.

blastRange1 :: (Location i FwdPosition Int) -> (Int,Int,Strand)
{-# Inline blastRange1 #-}
blastRange1 = f -- iso f t
  where
    f loc =
      let s = loc^.locPosition.fwdStart.to toInt1
          l = loc^.locSequence
          pm = loc^.locPosition.fwdStrand
      in  case pm of PlusStrand -> (s,s+l,pm) ; MinusStrand -> (s+l,s,pm)
--    t (x,y,pm) =
--      let s = fromInt1 x
--          l = 1 + abs (x-y)
--      in  Location (FwdPosition pm s) l



-- | For each element, attach the prefix as well. The @Int@ indicates the maximal prefix length to
-- attach.
--
-- @1 2 3 4@ -> @01 12 23 34@
--
-- TODO are we sure this is correct for @MinusStrand@?

attachPrefixes
  :: ( Monad m, ModifyLocation p s )
  => Int
  -> SP.Stream (SP.Of (PIS i p s)) m r
  -> SP.Stream (SP.Of (PIS i p s)) m r
{-# Inlinable attachPrefixes #-}
attachPrefixes k = SP.map (\(Just w) -> w) . SP.drop 1 . SP.scan go Nothing id
  where
    go Nothing = Just
    go (Just p) = Just . set pisPrefix (Just . locTakeEnd k $ view pisInfix p)



-- | For each element, attach the suffix as well.
--
-- @1 2 3 4@ -> @12 23 34 40@

attachSuffixes
  :: ( Monad m, ModifyLocation p s )
  => Int
  -> SP.Stream (SP.Of (PIS i p s)) m r
  -> SP.Stream (SP.Of (PIS i p s)) m r
{-# Inlinable attachSuffixes #-}
attachSuffixes k = loop Nothing
  where
    loop Nothing = \case
      SI.Return r -> SI.Return r
      SI.Effect m -> SI.Effect $ fmap (loop Nothing) m
      SI.Step (a SP.:> rest) -> loop (Just a) rest
    loop (Just p) = \case
      SI.Return r -> SI.Step (p SP.:> SI.Return r)
      SI.Effect m -> SI.Effect $ fmap (loop (Just p)) m
      SI.Step (a SP.:> rest) ->
        let p' = p & set pisSuffix (Just . locTake k $ view pisInfix a)
        in  SI.Step (p' SP.:> loop (Just a) rest)

