
-- | Strand information. A newtyped version, complete with serialization,
-- pattern synonyms, being a @PrimitiveArray@ index type, etc.
--
-- TODO will be expanded to encode biological sense information more
-- clearly: <http://en.wikipedia.org/wiki/Sense_%28molecular_biology%29>.

module Biobase.Types.Strand where

import Control.DeepSeq
import Control.Monad (guard)
import Data.Aeson
import Data.Binary
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Vector.Fusion.Stream.Monadic (Step(..))
import Data.Vector.Unboxed.Deriving
import GHC.Generics
import Test.QuickCheck
import Text.Printf

import Data.PrimitiveArray.Index.Class
import Data.PrimitiveArray.Vector.Compat



newtype Strand = Strand { getStrand :: Int }
  deriving (Eq,Ord,Generic)

instance Show Strand where
  show P = "+"
  show M = "-"

instance Read Strand where
  readsPrec _ xs = do
    ([pm],s) <- lex xs
    guard $ pm `elem` ("+-PMpm" :: String)
    return (go pm,s)
    where go x | x `elem` ("+Pp" :: String) = P
               | x `elem` ("-Mm" :: String) = M

instance Bounded Strand where
  minBound = P
  maxBound = M

instance Enum Strand where
  succ P = M
  succ M = error "succ M"
  pred M = P
  pred P = error "pred P"
  toEnum i | i>=0 && i<=1 = Strand i
  toEnum i                = error $ "toEnum (Strand)" ++ show i
  fromEnum = getStrand

instance NFData Strand

pattern P = Strand 0
pattern M = Strand 1

pattern Sense     = P
pattern AntiSense = M

instance Binary    Strand
instance Serialize Strand
instance ToJSON    Strand
instance FromJSON  Strand
instance Hashable  Strand

derivingUnbox "Strand"
  [t| Strand -> Int |]  [| getStrand |]  [| Strand |]

instance Index Strand where
  linearIndex _ _ (Strand z) = z
  {-# INLINE linearIndex #-}
  smallestLinearIndex (Strand l) = error "still needed?"
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (Strand h) = h
  {-# INLINE largestLinearIndex #-}
  size (_) (Strand h) = h + 1
  {-# INLINE size #-}
  inBounds (_) (Strand h) (Strand x) = 0<=x && x<=h
  {-# INLINE inBounds #-}

instance IndexStream z => IndexStream (z:.Strand) where
  streamUp (ls:.Strand lf) (hs:.Strand ht) = flatten mk step $ streamUp ls hs
    where mk z = return (z,lf)
          step (z,k)
            | k > ht    = return $ Done
            | otherwise = return $ Yield (z:.Strand k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.Strand lf) (hs:.Strand ht) = flatten mk step $ streamDown ls hs
    where mk z = return (z,ht)
          step (z,k)
            | k < lf    = return $ Done
            | otherwise = return $ Yield (z:.Strand k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream Strand

instance Arbitrary Strand where
  arbitrary = do
    b <- choose (0,1)
    return $ Strand b
  shrink (Strand j)
    | 0<j = [Strand $ j-1]
    | otherwise = []

