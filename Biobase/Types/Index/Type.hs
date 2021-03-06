
module Biobase.Types.Index.Type where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.Data (Data)
import           Data.Hashable (Hashable)
import           Data.Proxy
import           Data.Serialize (Serialize)
import           Data.Typeable (Typeable)
import           Data.Vector.Fusion.Stream.Monadic (Step(..), flatten)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import           GHC.TypeLits
import qualified Data.Ix as Ix
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           Test.QuickCheck
import           Text.Printf

import           Data.PrimitiveArray.Index.Class hiding (Index)
import qualified Data.PrimitiveArray.Index.Class as PA



-- | A linear @Int@-based index type.

newtype Index (t :: Nat) = Index { getIndex :: Int }
  deriving (Show,Read,Eq,Ord,Generic,Ix.Ix,Data,Typeable)

-- | Turn an 'Int' into an 'Index' safely.

index :: forall t . KnownNat t => Int -> Index t
index i = maybe (error $ printf "%d < Index %d\n" i n) id $ maybeIndex i
  where n = natVal (Proxy :: Proxy t)
{-# Inline index #-}

-- | Produce 'Just' and 'Index' or 'Nothing'.

maybeIndex :: forall t . KnownNat t => Int -> Maybe (Index t)
maybeIndex i
  | i >= n    = Just . Index $ i - n
  | otherwise = Nothing
  where n = fromIntegral $ natVal (Proxy :: Proxy t)
{-# Inline maybeIndex #-}

instance KnownNat t => Num (Index t) where
  Index a + Index b = error $ show (" Index.(+) not implemented, use (+.)",a,b) -- index $ a + b
  Index a - Index b = error $ show (" Index.(-) not implemented, use (-.)",a,b) -- index $ a - b
  Index a * Index b = error $ show (" Index.(*) not implemented", a,b) -- index $ a * b
  negate = error "Indices are natural numbers"
  abs = id
  signum = index . signum . getIndex
  fromInteger = index . fromIntegral
  {-# Inline fromInteger #-}

instance NFData (Index t) where
  rnf = rnf . getIndex
  {-# Inline rnf #-}

instance Binary    (Index t)
instance Serialize (Index t)
instance ToJSON    (Index t)
instance FromJSON  (Index t)
instance Hashable  (Index t)

derivingUnbox "Index"
  [t| forall t . Index t -> Int |]  [| getIndex |]  [| Index |]

instance forall t . KnownNat t => PA.Index (Index t) where
  newtype LimitType (Index t) = LtIndex Int
  linearIndex (LtIndex k) (Index z) = z
  {-# INLINE linearIndex #-}
  size (LtIndex h) = h + 1
  {-# INLINE size #-}
  inBounds (LtIndex h) (Index x) = 0<=x && x<=h
  {-# INLINE inBounds #-}
  zeroBound = Index 0
  {-# Inline zeroBound #-}
  zeroBound' = LtIndex 0
  {-# Inline zeroBound' #-}
  totalSize (LtIndex k) = [fromIntegral k]
  {-# Inline totalSize #-}
  fromLinearIndex _ = Index
  {-# Inline [0] fromLinearIndex #-}
  showBound (LtIndex k) = ["LtIndex " ++ show k]
  showIndex (Index k) = ["Index " ++ show k]

instance (KnownNat t, IndexStream z) ⇒ IndexStream (z:.Index t) where
  streamUp (ls:..LtIndex lf) (hs:..LtIndex ht) = flatten mk step $ streamUp ls hs
    where mk z = return (z,lf)
          step (z,k)
            | k > ht    = return $ Done
            | otherwise = return $ Yield (z:.Index k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:..LtIndex lf) (hs:..LtIndex ht) = flatten mk step $ streamDown ls hs
    where mk z = return (z,ht)
          step (z,k)
            | k < lf    = return $ Done
            | otherwise = return $ Yield (z:.Index k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance (KnownNat t) ⇒ IndexStream (Index t) where
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# INLINE streamUp #-}
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# INLINE streamDown #-}

instance Arbitrary (Index t) where
  arbitrary = Index <$> arbitrary
  shrink (Index j) = map Index $ shrink j

