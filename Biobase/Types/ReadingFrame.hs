
module Biobase.Types.ReadingFrame where

import GHC.Generics

import Biobase.Types.Index (Index, toInt0)



-- | The Reading frame. Sequence indexing starts at position 1, which starts
-- reading frame 1. Reading frame 2 and 3 start at position 2 and 3
-- respectively.
--
-- The reading frame should be constructed from an @Index 1@ with a smart
-- constructor to get the frame calculation right.

newtype ReadingFrame = ReadingFrame { getReadingFrame ∷ Int }
  deriving (Eq,Ord,Generic)

nextReadingFrame ∷ ReadingFrame → ReadingFrame
{-# Inline nextReadingFrame #-}
nextReadingFrame (ReadingFrame rf) = ReadingFrame $ rf `mod` 3 + 1

prevReadingFrame ∷ ReadingFrame → ReadingFrame
{-# Inline prevReadingFrame #-}
prevReadingFrame (ReadingFrame rf) = ReadingFrame $ rf `mod` 3 + 2

-- |
--
-- TODO should this be a type class, since we might reasonably want to
-- construct from a number of possible indices?

fromIndex ∷ Index 1 → ReadingFrame
{-# Inline fromIndex #-}
fromIndex i = ReadingFrame $ (toInt0 i `mod` 3) + 1

