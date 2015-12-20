
-- | Biological sequence data is oftentimes indexed either @0-@ or
-- @1-@based. The @Index@ type developed provides static guarantees that
-- there is no confusion what index is in use.
--
-- This module does not export the ctor @Index@. If you want to (unsafely)
-- use it, import @Biobase.Types.Index.Type@ directly. Use @fromInt0@ to
-- make clear that you count from 0 and transform to an @Index t@. I.e.
-- @fromInt0 0 :: Index 1@ yields the lowest 1-base index.

module Biobase.Types.Index
  ( module Biobase.Types.Index
  , getIndex
  , index
  , maybeIndex
  , Index
  ) where

import Data.Proxy
import GHC.TypeLits
import Text.Printf

import Biobase.Types.Index.Type



-- | Uses 'index' to guarantee that the 'Index' is ok.

checkIndex :: forall t . KnownNat t => Index t -> Index t
checkIndex i@(Index z)
  | z >= 0    = i
  | otherwise = error $ printf "%d < Index %d\n" (z+n) n
  where n :: Int = fromIntegral $ natVal (Proxy :: Proxy t)
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
(+.) i n = checkIndex $ unsafePlus i n
{-# Inline (+.) #-}

-- | Unsafe plus.

unsafePlus :: forall t . KnownNat t => Index t -> Int -> Index t
unsafePlus i n = Index $ getIndex i + n
{-# Inline unsafePlus #-}

-- | Helper function that allows @subtraction@ of an 'Index' and an 'Int',
-- with the 'Int' on the right.

(-.) :: forall t . KnownNat t => Index t -> Int -> Index t
(-.) i n = checkIndex $ unsafeMinus i n
{-# Inline (-.) #-}

-- | Delta between two 'Index' points.

delta :: forall t . KnownNat t => Index t -> Index t -> Int
delta i j = abs . getIndex $ i - j
{-# Inline delta #-}

-- | Unsafe minus.

unsafeMinus :: forall t . KnownNat t => Index t -> Int -> Index t
unsafeMinus i n = Index $ getIndex i - n
{-# Inline unsafeMinus #-}

-- | Return the index as an @Int@-style index that is zero-based.

toInt0 :: forall t . KnownNat t => Index t -> Int
toInt0 = getIndex
{-# Inline toInt0 #-}

-- | As an index from an @Int@-style zero-based one.
--
-- TODO We might want to check that the argument is @[0..]@.

fromInt0 :: forall t . KnownNat t => Int -> Index t
fromInt0 i
  | i >= 0    = Index i
  | otherwise = error "fromInt0 needs an Int >= 0"
  where t = fromIntegral $ natVal (Proxy :: Proxy t)
{-# Inline fromInt0 #-}

-- | Zero-based indices.

type I0 = Index 0

-- | One-based indices.

type I1 = Index 1

