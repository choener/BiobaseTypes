
module Biobase.Types.Index.Type where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.Hashable (Hashable)
import           Data.Proxy
import           Data.Serialize
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import           GHC.TypeLits
import qualified Data.Ix as Ix
import           Text.Printf



-- | A linear @Int@-based index type.

newtype Index (t :: Nat) = Index { getIndex :: Int }
  deriving (Show,Read,Eq,Ord,Generic,Ix.Ix)

-- | Turn an 'Int' into an 'Index' safely.

index :: forall t . KnownNat t => Int -> Index t
index i = maybe (error $ printf "%d < Index %d\n" i n) id $ maybeIndex i
  where n = natVal (Proxy :: Proxy t)
{-# Inline index #-}

-- | Produce 'Just' and 'Index' or 'Nothing'.

maybeIndex :: forall t . KnownNat t => Int -> Maybe (Index t)
maybeIndex i
  | i >= n    = Just $ Index i
  | otherwise = Nothing
  where n = fromIntegral $ natVal (Proxy :: Proxy t)
{-# Inline maybeIndex #-}

instance KnownNat t => Num (Index t) where
  Index a + Index b = index $ a + b
  Index a - Index b = index $ a - b
  Index a * Index b = index $ a * b
  negate = error "Indices are natural numbers"
  abs = id
  signum = index . signum . getIndex
  fromInteger = index . fromIntegral

instance NFData (Index t) where
  rnf = rnf . getIndex
  {-# Inline rnf #-}

instance Binary    (Index t)
instance Serialize (Index t)
instance ToJSON    (Index t)
instance FromJSON  (Index t)
instance Hashable  (Index t)

derivingUnbox "Index"
  [t| forall t . Index t -> Int |]
  [|  getIndex       |]
  [|  Index          |]

