
-- | Annotate the genomic @position@ of features or elements. A @position@ has strand information,
-- and different ways to encode where a feature is located. The @position@ points to the first
-- element (e.g. nucleotide).
--
-- Together with the 'Biobase.Types.Location' module, it becomes possible to annotate substrings.

module Biobase.Types.Position where

import Control.DeepSeq
import Control.Lens hiding (Index, index)
import Data.Data
import GHC.Generics (Generic)
import GHC.TypeNats
import Prelude hiding (length)
import Text.Printf

import Biobase.Types.Index
import Biobase.Types.Strand
import Data.Info

{-

-- | Location information.

data Location = Location
  { _lStrand :: !Strand
  -- ^ On which strand are we
  , _lStart  :: !(Index 0)
  -- ^ Start, 0-based
  , _lLength :: !Int
  -- ^ number of characters in this location
  , _lTotalLength :: !Int
  -- ^ the total length of the "contig" (or whatever) this location is positioned in.
  } deriving (Eq,Ord,Read,Show,Generic)
makeLenses ''Location
makePrisms ''Location

instance NFData Location

instance Semigroup Location where
  x <> y = let f z = z { _lLength = _lLength x + _lLength y }
    in case x^.lStrand of
      MinusStrand  -> f y
      _otherStrand -> f x
  {-# Inline (<>) #-}

--instance Reversing Location where
--  {-# Inline reversing #-}
--  reversing = undefined


-- | An isomorphism between locations, and triples of @Strand,Start,End@, where
-- end is inclusive. For @length==0@ locations, this will mean @start<end@ on
-- the plus strand.
--
-- This should hold for all @k@, in @Index k@.

startEndInclusive :: (KnownNat k) => Iso' Location (Strand, (Index k, Index k), Int)
{-# Inline startEndInclusive #-}
startEndInclusive = iso l2r r2l
  where l2r z = let s = z^.lStrand; f = z^.lStart; l = z^.lLength
                in  (s, (reIndex f, reIndex $ f +. l -. 1), z^.lTotalLength)
        r2l (s,(f,t),ttl) = Location s (reIndex f) (delta f t + 1) ttl

-}



-- | During streaming construction, it is possible that we know a feature is on the @-@ strand, but
-- the length of the contig is not known yet. In that case, 'FwdPosition' allows expressing the hit
-- in the coordinate system of the plus strand. Tools like blast do something similar, and express
-- locations on the minus as @y-x@ with @y>x@.
--
-- @
-- 0123456789
--  >-->
--      <--<
-- 9876543210
-- @
--
-- 

data FwdPosition
  -- | "Plus"-based location.
  = FwdPosition
      { _fwdStrand :: !Strand
      -- ^ Strand we are on
      , _fwdStart  :: !(Index 0)
      -- ^ Start of the hit on the plus strand
      }
  deriving (Eq,Ord,Read,Show,Data,Typeable,Generic)
makeLenses ''FwdPosition
makePrisms ''FwdPosition

instance NFData FwdPosition

instance Info FwdPosition where
  info (FwdPosition s x) = printf "%s %d" (show s) (toInt0 x)

-- | Reversing a reversible location means moving the start to the end.

instance Reversing FwdPosition where
  {-# Inline reversing #-}
  reversing x = case x^.fwdStrand of
    PlusStrand    -> set fwdStrand MinusStrand $ x
    MinusStrand   -> set fwdStrand PlusStrand  $ x
    UnknownStrand -> x

{-


-- | Combining two FwdLocations yields the sum of their lengths. This assumes
-- that @x@ and @y@ are next to each other, or that it is ok if the @y@
-- @fwdStart@ information may be lost.
--
-- TODO provide associativity test in @properties@.

instance Semigroup FwdLocation where
  x <> y = over fwdLength (+ view fwdLength y) x
  {-# Inline (<>) #-}

instance ModifyLocation FwdLocation where
  locMoveLeftEnd k = over fwdStart (+. k) . over fwdLength (subtract k)
  locMoveRightEnd k = over fwdLength (+k)

-- | Given a location, take at most @k@ elements, and return a location after
-- this change.

fwdLocationTake :: Int -> FwdLocation -> FwdLocation
{-# Inline fwdLocationTake #-}
fwdLocationTake k' x =
  let l = x^.fwdLength
      k = max 0 $ min k' l      -- deal with at most the length of the location
  in case x^.fwdStrand of
    MinusStrand  -> set fwdLength k $ over fwdStart (+. (l-k)) x
    _otherStrand -> set fwdLength k $                          x

-- | Given a location, drop at most @k@ elements, and return a location after
-- this change.
--
-- Note that @fwdLocationDrop 4 (FwdLocation PlusStrand 0 4) == FwdLocation 4 0@

fwdLocationDrop :: Int -> FwdLocation -> FwdLocation
{-# Inline fwdLocationDrop #-}
fwdLocationDrop k' x =
  let l = x^.fwdLength
      k = max 0 $ min k' l
  in case x^.fwdStrand of
    MinusStrand  -> set fwdLength (l-k) $                            x
    _otherStrand -> set fwdLength (l-k) $ over fwdStart (+. min k l) x

-- -- An isomorphism between a 'Location' and the pair @('FwdLocation',Int)@
-- -- exists.
-- 
-- locationPartial :: Iso' Location (FwdLocation,Int)
-- {-# Inline locationPartial #-}
-- locationPartial = iso l2r r2l where
--   l2r l = undefined
--   r2l (p,z) = undefined

-}

