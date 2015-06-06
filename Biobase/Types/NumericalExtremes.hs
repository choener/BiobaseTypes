
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
-- @maxFinite >= extremelyLarge >= veryLarge@
--
-- @veryLarge >= verySmall@
--
-- @verySmall >= extremelySmall >= minFinite@.
--
-- TODO the small stuff should actually be around zero, but positive and go
-- into @NumericalEpsilon@. Here we should actually use other names.

class NumericalExtremes x where
  -- | Largest finite number
  maxFinite       :: x
  -- | Smallest finite number
  minFinite       :: x
  -- | Around @1/100@ of the largest finite number
  veryLarge       :: x
  -- | Around @1/100@ of the smallest finite number
  verySmall       :: x
  -- | Around @1/ 10@ of the largest finite number
  extremelyLarge  :: x
  -- | Around @1/ 10@ of the smallest finite number
  extremelySmall  :: x

-- | Small numbers.

class NumericalEpsilon x where
  -- | Smallest positive number @/= 0.0@.
  epsilon         :: x



instance NumericalExtremes Int where
  maxFinite      = maxBound
  minFinite      = minBound
  veryLarge      = maxBound `div` 100
  verySmall      = minBound `div` 100
  extremelyLarge = maxBound `div`  10
  extremelySmall = minBound `div`  10
  {-# Inline veryLarge      #-}
  {-# Inline verySmall      #-}
  {-# Inline extremelyLarge #-}
  {-# Inline extremelySmall #-}



instance NumericalExtremes Double where
  maxFinite      =  1.79e+308
  minFinite      = -1.79e+308
  veryLarge      = maxFinite / 100
  verySmall      = minFinite / 100
  extremelyLarge = maxFinite /  10
  extremelySmall = minFinite /  10
  {-# Inline veryLarge      #-}
  {-# Inline verySmall      #-}
  {-# Inline extremelyLarge #-}
  {-# Inline extremelySmall #-}

instance NumericalEpsilon Double where
  epsilon = 2.2e-16
  {-# Inline epsilon #-}

