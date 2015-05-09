
-- | Biological sequence data is oftentimes indexed either @0-@ or
-- @1-@based. The @Index@ type developed provides static guarantees that
-- there is no confusion what index is in use.
--
-- This module does not export the ctor @Index@. If you want to (unsafely)
-- use it, import @Biobase.Types.Index.Type@ directly.

module Biobase.Types.Index
  ( Index
  , index
  , getIndex
  , maybeIndex
  , checkIndex
  , reIndex
  , (+.)
  , (-.)
  , fromInt
  , toInt
  ) where

import Data.Proxy
import GHC.TypeLits

import Biobase.Types.Index.Type



-- | Uses 'index' to guarantee that the 'Index' is ok.

checkIndex :: Index t -> Index t
checkIndex = Index . getIndex
{-# Inline checkIndex #-}

-- | Re-Index an index of type @Index n@ as @Index m@. This is always safe,
-- as @0 :: Index 0@ gives @1 :: Index 1@ for example. I.e. valid indices
-- become valid indices.

reIndex :: forall n m . (KnownNat n, KnownNat m) => Index n -> Index m
reIndex (Index i) = Index $ i - n + m
  where n = fromIntegral $ natVal (Proxy :: Proxy n)
        m = fromIntegral $ natVal (Proxy :: Proxy m)
{-# Inline reIndex #-}

-- | Helper function that allows @addition@ of an 'Index' and an 'Int',
-- with the 'Int' on the right.

(+.) :: forall t . KnownNat t => Index t -> Int -> Index t
(+.) i n = i + Index n
{-# Inline (+.) #-}

-- | Helper function that allows @subtraction@ of an 'Index' and an 'Int',
-- with the 'Int' on the right.

(-.) :: forall t . KnownNat t => Index t -> Int -> Index t
(-.) i n = i - Index n
{-# Inline (-.) #-}

-- | Return the index as an @Int@-style index that is zero-based.

toInt :: forall t . KnownNat t => Index t -> Int
toInt = subtract t . getIndex
  where t = fromIntegral $ natVal (Proxy :: Proxy t)
{-# Inline toInt #-}

-- | As an index from an @Int@-style zero-based one.
--
-- TODO We might want to check that the argument is @[0..]@.

fromInt :: forall t . KnownNat t => Int -> Index t
fromInt = Index . (+ t)
  where t = fromIntegral $ natVal (Proxy :: Proxy t)
{-# Inline fromInt #-}

