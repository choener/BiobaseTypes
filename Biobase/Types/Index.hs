
-- | Biological sequence data is oftentimes indexed either @0-@ or
-- @1-@based. The @Index@ type developed provides static guarantees that
-- there is no confusion what index is in use.
--
-- This module does not export the ctor @Index@. If you want to (unsafely)
-- use it, import @Biobase.Types.Index.Type@ directly. Use @fromInt0@ to
-- make clear that you count from 0 and transform to an @Index t@. I.e.
-- @fromInt0 0 :: Index 1@ yields the lowest 1-base index.
--
-- Note that internally, every lowest index starts at @0 :: Int@.

module Biobase.Types.Index
  ( module Biobase.Types.Index
  , getIndex
  , index
  , maybeIndex
  , Index
  ) where

import Data.Coerce
import Data.Proxy
import GHC.TypeLits
import Text.Printf

import Biobase.Types.Index.Type -- hiding (getIndex)
import qualified Biobase.Types.Index.Type as IT



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

reIndex ∷ Index n → Index m
{-# Inline reIndex #-}
reIndex = coerce
--reIndex :: forall n m . (KnownNat n, KnownNat m) => Index n -> Index m
--reIndex (Index i) = Index $ i - n + m
--  where n = fromIntegral $ natVal (Proxy :: Proxy n)
--        m = fromIntegral $ natVal (Proxy :: Proxy m)

-- | Helper function that allows @addition@ of an 'Index' and an 'Int',
-- with the 'Int' on the right.

(+.) :: forall t . KnownNat t => Index t -> Int -> Index t
(+.) i n = checkIndex $ unsafePlus i n
{-# Inline (+.) #-}

-- | Helper function that allows @subtraction@ of an 'Index' and an 'Int',
-- with the 'Int' on the right.

(-.) :: forall t . KnownNat t => Index t -> Int -> Index t
(-.) i n = checkIndex $ unsafePlus i (negate n)
{-# Inline (-.) #-}

-- | Unsafe plus.

unsafePlus :: forall t . KnownNat t => Index t -> Int -> Index t
unsafePlus i n = Index $ IT.getIndex i + n
{-# Inline unsafePlus #-}

-- | Delta between two 'Index' points.

delta :: forall t . KnownNat t => Index t -> Index t -> Int
delta (Index i) (Index j) = abs $ i - j
{-# Inline delta #-}

toInt ∷ forall t . KnownNat t ⇒ Index t → Int
{-# Inline toInt #-}
toInt i = IT.getIndex i + (fromIntegral $ natVal (Proxy ∷ Proxy t))

-- | Return the index as an @Int@-style index that is zero-based.

toInt0 :: forall t . KnownNat t => Index t -> Int
toInt0 = IT.getIndex
{-# Inline toInt0 #-}

-- | Return the index as an @Int@-style index that is one-based.

toInt1 ∷ forall t . KnownNat t ⇒ Index t → Int
{-# Inline toInt1 #-}
toInt1 = (+1) . toInt0

fromInt1 ∷ forall t . KnownNat t ⇒ Int → Index t
{-# Inline fromInt1 #-}
fromInt1 = fromInt0 . (subtract 1)

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

