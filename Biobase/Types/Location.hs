
-- | Annotate the genomic @Location@ of features or elements. A @Location@ is
-- always contiguous, using strand, 0-based position, and length.
-- Transformation to different systems of annotation is made possible.

module Biobase.Types.Location where

import Control.Lens hiding (Index, index)
import GHC.Generics (Generic)
import GHC.TypeNats
import Prelude hiding (length)

import Biobase.Types.Index
import Biobase.Types.Strand



-- | Location information.

data Location = Location
  { _strand ∷ !Strand
  -- ^ On which strand are we
  , _start  ∷ !(Index 0)
  -- ^ Start, 0-based
  , _length ∷ !Int
  -- ^ Total number of characters
  } deriving (Eq,Ord,Read,Show,Generic)
makeLenses ''Location
makePrisms ''Location

-- | An isomorphism between locations, and triples of @Strand,Start,End@, where
-- end is inclusive. For @length==0@ locations, this will mean @start<end@ on
-- the plus strand.
--
-- This should hold for all @k@, in @Index k@.

startEndInclusive ∷ (KnownNat k) ⇒ Iso' Location (Strand, Index k, Index k)
{-# Inline startEndInclusive #-}
startEndInclusive = iso l2r r2l
  where l2r z = let s = z^.strand; f = z^.start; l = z^.length
                in  (s, reIndex f, reIndex $ f +. l -. 1)
        r2l (s,f,t) = Location s (reIndex f) (delta f t + 1)



-- | During streaming construction, it is possible that we know a feature is on
-- the @-@ strand, but the length of the contig is not known yet.
--
-- @
-- 0         1         2
-- 012345678901234567890
--   >---                    PlusStranded 2 4       Location +  2 4
--      ---<                 RevCompStranded 8 4    Location - 12 4
-- 098765432109876543210
-- 2         1         0
-- @
--
-- A windowed streamed location will need to move @slStart@ by the offset.

data StreamedLocation
  -- | We are on the plus strand.
  = PlusStranded
    { _slStart  ∷ !(Index 0)
    , _slLength ∷ !Int
    }
  -- | We are on the complement of the plus strand, but do not know how far
  -- from the start, since we still stream.
  | RevCompStranded
    { _slStart  ∷ !(Index 0)
      -- ^ start of the sequence on the complement. Start is the *rightmost*
      -- element when seen from the PlusStrand (which we have
      -- Reverse-complemented).
    , _slLength ∷ !Int
    }
makeLenses ''StreamedLocation
makePrisms ''StreamedLocation

-- From
-- certain locations, we can go back to streamed locations. Strandedness is
-- always in terms of something we just stream, meaning that non-@+/-@
-- strandedness in locations has no counterpart as a streamed location.
--

locationStreamed
  ∷ Int
  -- ^ contig length
  → Prism' Location StreamedLocation
{-# Inline locationStreamed #-}
locationStreamed len = prism' r2l l2r
  where r2l = fromStreamed len
        l2r Location{..} = case _strand of
          PlusStrand  → Just $ PlusStranded _start _length
          MinusStrand → Just $ RevCompStranded (index $ len - getIndex _start +1) _length
          otherwise   → Nothing

-- | Given a 'StreamedLocation' we can calculate the actual 'Location'. This is
-- just a function, or a @Getter@ with @to ...@.
--
-- The total length of the contig is always needed (but actually only required
-- when given a 'RevCompStranded'.

fromStreamed ∷ Int → StreamedLocation → Location
{-# Inline fromStreamed #-}
fromStreamed len = \case
  PlusStranded{..}    → Location PlusStrand _slStart _slLength
  RevCompStranded{..} → Location MinusStrand (index $ len - getIndex _slStart -1) _slLength

