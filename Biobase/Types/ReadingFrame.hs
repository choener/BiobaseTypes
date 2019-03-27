
-- | Stranded reading frames.

module Biobase.Types.ReadingFrame where

import Control.Lens hiding (Index)
import GHC.Generics hiding (from)

import Biobase.Types.Index (Index, toInt0)
import Biobase.Types.Strand



-- | The Reading frame. Sequence indexing starts at position 1, which starts
-- reading frame 1. Reading frame 2 and 3 start at position 2 and 3
-- respectively.

newtype ReadingFrame = ReadingFrame { getReadingFrame ∷ Int }
  deriving (Eq,Ord,Generic,Show)
makeWrapped ''ReadingFrame

-- | Convert between @+1 ... +3@ and @ReadingFrame@.

rf ∷ Prism' Int ReadingFrame
{-# Inline rf #-}
rf = prism' getReadingFrame $ \k → let ak = abs k in
  if (ak <=  3 && ak >= 1) then Just (ReadingFrame k) else Nothing

-- | A lens for the strand

strandRF ∷ Lens' ReadingFrame Strand
{-# Inline strandRF #-}
strandRF = lens (\(ReadingFrame k) → if k < 0 then MinusStrand else PlusStrand)
                (\(ReadingFrame k) s → ReadingFrame $ if s == PlusStrand then abs k else (negate $ abs k))

-- |
--
-- @pred@ and @succ@ are correct, if the input is a legal 'ReadingFrame'.

instance Enum ReadingFrame where
  {-# Inline toEnum #-}
  toEnum k = case k^?rf of Just rf → rf ; Nothing → error $ show k ++ " is not a legal reading frame"
  {-# Inline fromEnum #-}
  fromEnum = getReadingFrame

-- |
--
-- TODO should this be a type class, since we might reasonably want to
-- construct from a number of possible indices?

fromIndex ∷ Index 1 → ReadingFrame
{-# Inline fromIndex #-}
fromIndex i = ReadingFrame $ (toInt0 i `mod` 3) + 1

