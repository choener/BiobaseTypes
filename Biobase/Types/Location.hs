
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
  { _lStrand ∷ !Strand
  -- ^ On which strand are we
  , _lStart  ∷ !(Index 0)
  -- ^ Start, 0-based
  , _lLength ∷ !Int
  -- ^ number of characters in this location
  , _lTotalLength ∷ !Int
  -- ^ the total length of the "contig" (or whatever) this location is positioned in.
  } deriving (Eq,Ord,Read,Show,Generic)
makeLenses ''Location
makePrisms ''Location

instance Reversing Location where
  {-# Inline reversing #-}
  reversing = undefined


-- | An isomorphism between locations, and triples of @Strand,Start,End@, where
-- end is inclusive. For @length==0@ locations, this will mean @start<end@ on
-- the plus strand.
--
-- This should hold for all @k@, in @Index k@.

startEndInclusive ∷ (KnownNat k) ⇒ Iso' Location (Strand, (Index k, Index k), Int)
{-# Inline startEndInclusive #-}
startEndInclusive = iso l2r r2l
  where l2r z = let s = z^.lStrand; f = z^.lStart; l = z^.lLength
                in  (s, (reIndex f, reIndex $ f +. l -. 1), z^.lTotalLength)
        r2l (s,(f,t),ttl) = Location s (reIndex f) (delta f t + 1) ttl



-- | During streaming construction, it is possible that we know a feature is on
-- the @-@ strand, but the length of the contig is not known yet.
--
-- @
-- 0         1         2
-- 012345678901234567890
--   >---                    +        2 4    Location +  2 4
--      <---                 Reversed 5 4    Location - 12 4
-- 098765432109876543210
-- 2         1         0
-- @
--
-- 

data PartialLocation
  -- | Location, when it is not yet known how long the contig will be.
  = PartialLocation
      { _plStrand ∷ !Strand
      , _plStart  ∷ !(Index 0)
      , _plLength ∷ !Int
      }
  -- | The reversed strand. However, we have an @plEnd@, not a @plStart@ now!
  | ReversedPartialLocation
      { _plStrand ∷ !Strand
      , _plEnd    ∷ !(Index 0)
      , _plLength ∷ !Int
      }
  deriving (Eq,Ord,Read,Show,Generic)
makeLenses ''PartialLocation
makePrisms ''PartialLocation

-- | Reversing a reversible location means moving the start to the end.

instance Reversing PartialLocation where
  {-# Inline reversing #-}
  reversing = \case
    PartialLocation s t l → ReversedPartialLocation (s^.reversed) t l

-- An isomorphism between a 'Location' and the pair @('PartialLocation',Int)@
-- exists.

locationPartial ∷ Iso' Location (PartialLocation,Int)
{-# Inline locationPartial #-}
locationPartial = iso l2r r2l where
  l2r l = (PartialLocation (view lStrand l) (view lStart l) (view lLength l), l^.lTotalLength)
  r2l (p,z) = case p of PartialLocation s t l → Location s t l z
                        ReversedPartialLocation s e l
                          | s `elem` [PlusStrand,MinusStrand] → Location s (index $ z- getIndex e -l) l z
                          | otherwise                         → Location s e l z

