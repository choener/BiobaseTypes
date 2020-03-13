
-- | Annotate the genomic @Location@ of features or elements. A @Location@ is
-- always contiguous, using strand, 0-based position, and length.
-- Transformation to different systems of annotation is made possible.

module Biobase.Types.Location where

import Control.DeepSeq
import Control.Lens hiding (Index, index)
import Data.Coerce
import GHC.Generics (Generic)
import GHC.TypeNats
import Prelude hiding (length)
import qualified Data.ByteString as BS
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
makeLenses ''Location

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

instance Reversing (Location i FwdPosition (BioSequence w)) where
  {-# Inline reversing #-}
  reversing = over (locSequence._BioSequence) BS.reverse . over (locPosition) reversing

instance Complement (BioSequence w) => Complement (Location i FwdPosition (BioSequence w)) where
  {-# Inline complement #-}
  complement = iso f f
    where f = over locSequence (view complement)

instance (Info (BioSequence w)) => Info (Location i FwdPosition (BioSequence w)) where
  info loc = printf "%s %s %s" (loc^.locIdentifier^.to show) (show $ loc^.locPosition) (loc^.locSequence.to info)



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

