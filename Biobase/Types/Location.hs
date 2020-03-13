
-- | Annotate the genomic @Location@ of features or elements. A @Location@ is
-- always contiguous, using strand, 0-based position, and length.
-- Transformation to different systems of annotation is made possible.

module Biobase.Types.Location where

import Control.DeepSeq
import Control.Lens hiding (Index, index)
import GHC.Generics (Generic)
import GHC.TypeNats
import Prelude hiding (length)
import qualified Data.ByteString as BS

import Biobase.Types.BioSequence
import Biobase.Types.Index
import Biobase.Types.Position
import Biobase.Types.Strand




-- | Operations on locations.

class ModifyLocation posTy seqTy where
  -- | Append to the left.
  locAppendLeft  :: seqTy -> Location posTy seqTy -> Location posTy seqTy
  -- | Append to the right.
  locAppendRight :: seqTy -> Location posTy seqTy -> Location posTy seqTy
  -- | Split a location.
  locSplitAt  :: Int -> Location posTy seqTy -> (Location posTy seqTy, Location posTy seqTy)
  -- | Length of location
  locLength :: Location posTy seqTy -> Int

locTake k = fst . locSplitAt k

locTakeEnd k loc = let l = locLength loc in snd $ locSplitAt (l-k) loc

locDrop k = snd . locSplitAt k

locDropEnd k loc = let l = locLength loc in fst $ locSplitAt (l-k) loc



data Location posTy seqTy = Location
  { _locPosition  :: !posTy
  , _locSequence  :: !seqTy
  }
makeLenses ''Location

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



{-

-- | Provides a range in a notation as used by blast, for example. This
-- isomorphism can translate back as well. @FwdLocation - 8 4 ^. blastRange1 ==
-- 9 6 MinusStrand@, since these ranges are 1-based and start and end included.

blastRange1 :: Iso' FwdLocation (Int, Int, Strand)
{-# Inline blastRange1 #-}
blastRange1 = iso f t where
  f FwdLocation{..} =
      let s = toInt1 _fwdStart
          l = _fwdLength -1
      in  case _fwdStrand of
      PlusStrand  -> (s, s+l,_fwdStrand)
      MinusStrand -> (s+l, s,_fwdStrand)
  t (x,y,str) =
      let s = fromInt1 x
          l = 1 + abs (x-y)
      in  FwdLocation str s l

-}

