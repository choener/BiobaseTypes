
-- | For some values, we want to have different kind of extreme values.
-- Consider a @Double@ representing an energy. We want @near infinities@
-- that do not lead to numeric problems.
--
-- TODO benchmark different extremes and their interplay with algebraic
-- operations.
--
-- TODO consider the @ieee754@ package

module Biobase.Types.NumericalExtremes where



-- | Very large and small numbers with some numerical safety to @1/0@ or
-- @maxBound@ (depending on if we are @Integral@ or @RealFloat@.
--
-- We have:
--
-- @maxFinite >= maxExtreme >= maxLarge@
--
-- @maxLarge >= minLarge@
--
-- @minLarge >= minExtreme >= minFinite@.

class NumericalExtremes x where
  maxFinite   :: x  -- ^ Largest finite number
  minFinite   :: x  -- ^ Smallest finite number
  maxExtreme  :: x  -- ^ Around @1/ 10@ of the largest finite number
  minExtreme  :: x  -- ^ Around @1/ 10@ of the smallest finite number
  maxLarge    :: x  -- ^ Around @1/100@ of the largest finite number
  minLarge    :: x  -- ^ Around @1/100@ of the smallest finite number

-- | Small numbers.

class NumericalEpsilon x where
  epsilon   :: x  -- ^ Smallest positive number @/= 0.0@.



instance NumericalExtremes Int where
  maxFinite  = maxBound
  minFinite  = minBound
  maxLarge   = maxBound `div` 100
  minLarge   = minBound `div` 100
  maxExtreme = maxBound `div`  10
  minExtreme = minBound `div`  10
  {-# Inline maxFinite  #-}
  {-# Inline minFinite  #-}
  {-# Inline maxExtreme #-}
  {-# Inline minExtreme #-}
  {-# Inline maxLarge   #-}
  {-# Inline minLarge   #-}



instance NumericalExtremes Double where
  maxFinite  =  1.79e+308
  minFinite  = -1.79e+308
  maxExtreme =  1.79e+307
  minExtreme = -1.79e+307
  maxLarge   =  1.79e+306
  minLarge   = -1.79e+306
  {-# Inline maxFinite  #-}
  {-# Inline minFinite  #-}
  {-# Inline maxExtreme #-}
  {-# Inline minExtreme #-}
  {-# Inline maxLarge   #-}
  {-# Inline minLarge   #-}



instance NumericalEpsilon Double where
  epsilon = 2.2e-16
  {-# Inline epsilon #-}

