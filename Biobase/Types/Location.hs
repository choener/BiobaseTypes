
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

instance Semigroup Location where
  x <> y = let f z = z { _lLength = _lLength x + _lLength y }
    in case x^.lStrand of
      MinusStrand  → f y
      _otherStrand → f x
  {-# Inline (<>) #-}

--instance Reversing Location where
--  {-# Inline reversing #-}
--  reversing = undefined


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
-- the @-@ strand, but the length of the contig is not known yet. In that case,
-- 'FwdLocation' allows expressing the hit in the coordinate system of the plus
-- strand. Tools like blast do something similar, and express locations on the
-- minus as @y-x@ with @y>x@. The example below has
-- @FwdLocation + 2 4@ and @FwdLocation - 8 4@.
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

data FwdLocation
  -- | Location, when it is not yet known how long the contig will be.
  = FwdLocation
      { _fwdStrand ∷ !Strand
      -- ^ the strand we are on.
      , _fwdStart  ∷ !(Index 0)
      -- ^ start of the hit
      , _fwdLength ∷ !Int
      -- ^ length of the hit
      }
  deriving (Eq,Ord,Read,Show,Generic)
makeLenses ''FwdLocation
makePrisms ''FwdLocation

-- | Combining two FwdLocations yields the sum of their lengths. This assumes
-- that @x@ and @y@ are next to each other.
--
-- TODO consider if that makes sense

instance Semigroup FwdLocation where
  x <> y = let f z = z { _fwdLength = _fwdLength x + _fwdLength y }
    in case x^.fwdStrand of
    MinusStrand → f y
    _otherStrand → f x
  {-# Inline (<>) #-}

-- | Given a left and a right (possibly negative) extension, modify the Location

extendLocation ∷ Int → Int → FwdLocation → FwdLocation
{-# Inline extendLocation #-}
extendLocation l r fwd = case fwd^.fwdStrand of
  MinusStrand  → over fwdStart (+. r) $ over fwdLength (+(l+r)) fwd
  _otherStrand → over fwdStart (-. l) $ over fwdLength (+(l+r)) fwd

-- | Given a location, take at most @k@ elements, and return a location after
-- this change.

fwdLocationTake ∷ Int → FwdLocation → FwdLocation
{-# Inline fwdLocationTake #-}
fwdLocationTake k' l = let k = min k' $ l^.fwdLength
  in case l^.fwdStrand of
    MinusStrand  → over fwdStart (-. (l^.fwdLength - k)) . set fwdLength k $ l
    _otherStrand → set fwdLength k l

-- | Given a location, drop at most @k@ elements, and return a location after
-- this change.

fwdLocationDrop ∷ Int → FwdLocation → FwdLocation
{-# Inline fwdLocationDrop #-}
fwdLocationDrop k' l = let k = min k' $ l^.fwdLength
  in case l^.fwdStrand of
    MinusStrand  → over fwdLength (subtract k) l
    _otherStrand → over fwdStart (+. k) . over fwdLength (subtract k) $ l

-- | Provides a range in a notation as used by blast, for example. This
-- isomorphism can translate back as well. @FwdLocation - 8 4 ^. blastRange1 ==
-- 9 6 MinusStrand@, since these ranges are 1-based and start and end included.

blastRange1 ∷ Iso' FwdLocation (Int, Int, Strand)
{-# Inline blastRange1 #-}
blastRange1 = iso f t where
  f FwdLocation{..} =
      let s = toInt1 _fwdStart
          l = _fwdLength -1
      in  case _fwdStrand of
      PlusStrand  → (s, s+l,_fwdStrand)
      MinusStrand → (s, s-l,_fwdStrand)
  t (x,y,str) =
      let s = fromInt1 x
          l = abs (x-y) +1
      in  FwdLocation str s l

-- | Reversing a reversible location means moving the start to the end.

instance Reversing FwdLocation where
  {-# Inline reversing #-}
  reversing FwdLocation{..} = case _fwdStrand of
    PlusStrand  → FwdLocation MinusStrand (index $ _fwdLength-1) _fwdLength
    MinusStrand → FwdLocation PlusStrand  0                      _fwdLength

-- -- An isomorphism between a 'Location' and the pair @('FwdLocation',Int)@
-- -- exists.
-- 
-- locationPartial ∷ Iso' Location (FwdLocation,Int)
-- {-# Inline locationPartial #-}
-- locationPartial = iso l2r r2l where
--   l2r l = undefined
--   r2l (p,z) = undefined

