
-- | Strand information. A newtyped version, complete with serialization,
-- pattern synonyms, being a @PrimitiveArray@ index type, etc. The strand
-- information includes @+@, @-@, as well as the (GFF3) @.@ not stranded, and
-- @?@ for unknown strand information.
--
-- TODO will be expanded to encode biological sense information more clearly:
-- <http://en.wikipedia.org/wiki/Sense_%28molecular_biology%29>.

module Biobase.Types.Strand where

import Control.DeepSeq
import Control.Lens hiding (Index)
import Control.Monad (guard)
import Data.Aeson
import Data.Binary
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Typeable (Typeable)
import Data.Vector.Fusion.Stream.Monadic (Step(..), flatten)
import Data.Vector.Unboxed.Deriving
import GHC.Generics
import Test.QuickCheck
import Text.Printf

import Data.PrimitiveArray.Index.Class



-- | Encode strand information. 'PlusStrand' is defined as the strand encoded
-- in, say, the FASTA file. 'MinusStrand' hence is the reverse complement.

newtype Strand = Strand { getStrand :: Int }
  deriving (Eq,Ord,Generic,Data,Typeable)

instance Show Strand where
  show PlusStrand    = "PlusStrand"
  show MinusStrand   = "MinusStrand"
  show NotStranded   = "NotStranded"
  show UnknownStrand = "UnknownStrand"

instance Read Strand where
  readsPrec _ xs = do
    (pm,s) <- lex xs
    case pm of
      "PlusStrand" → return (PlusStrand, s)
      "MinusStrand" → return (MinusStrand, s)
      "NotStranded" → return (NotStranded, s)
      "UnknownStrand" → return (UnknownStrand, s)
      [x] | x `elem` ("+Pp" ∷ String) → return (PlusStrand,s)
          | x `elem` ("-Mm" ∷ String) → return (MinusStrand,s)
          | x `elem` ("."   ∷ String) → return (NotStranded,s)
          | x `elem` ("?"   ∷ String) → return (UnknownStrand,s)
      _ → []

instance Bounded Strand where
  minBound = PlusStrand
  maxBound = UnknownStrand

instance Enum Strand where
  succ (Strand k)
    | k <  0 = error "succ undefined strand"
    | k == 3 = error "succ UnknownStrand"
    | k >  3 = error "succ undefined strand"
    | otherwise = Strand (k+1)
  pred (Strand k)
    | k <  0 = error "pred undefined strand"
    | k == 0 = error "pred UnknownStrand"
    | k >  3 = error "pred undefined strand"
    | otherwise = Strand (k-1)
  toEnum i | i>=0 && i<=3 = Strand i
  toEnum i                = error $ "toEnum (Strand)" ++ show i
  fromEnum = getStrand

instance Reversing Strand where
  reversing PlusStrand  = MinusStrand
  reversing MinusStrand = PlusStrand
  reversing x           = x

pattern PlusStrand    = Strand 0
pattern MinusStrand   = Strand 1
pattern NotStranded   = Strand 2
pattern UnknownStrand = Strand 3

-- TODO Sense and Antisense are somewhat different

--pattern Sense     = P
--pattern AntiSense = M

instance Binary    Strand
instance Serialize Strand
instance ToJSON    Strand
instance FromJSON  Strand
instance Hashable  Strand
instance NFData    Strand

derivingUnbox "Strand"
  [t| Strand -> Int |]  [| getStrand |]  [| Strand |]

instance Index Strand where
  newtype (LimitType Strand) = LtStrand Strand
  linearIndex _ (Strand z) = z
  {-# INLINE linearIndex #-}
  size (LtStrand (Strand h)) = h + 1
  {-# INLINE size #-}
  inBounds (LtStrand (Strand h)) (Strand x) = 0<=x && x<=h
  {-# INLINE inBounds #-}
  zeroBound = Strand 0
  {-# Inline zeroBound #-}
  zeroBound' = LtStrand zeroBound
  {-# Inline zeroBound' #-}
  totalSize (LtStrand (Strand k)) = [ fromIntegral (fromEnum k + 1) ]
  {-# Inline totalSize #-}

instance IndexStream z => IndexStream (z:.Strand) where
  streamUp (ls:..LtStrand (Strand lf)) (hs:..LtStrand (Strand ht)) = flatten mk step $ streamUp ls hs
    where mk z = return (z,lf)
          step (z,k)
            | k > ht    = return $ Done
            | otherwise = return $ Yield (z:.Strand k) (z,k+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:..LtStrand (Strand lf)) (hs:..LtStrand (Strand ht)) = flatten mk step $ streamDown ls hs
    where mk z = return (z,ht)
          step (z,k)
            | k < lf    = return $ Done
            | otherwise = return $ Yield (z:.Strand k) (z,k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

-- instance IndexStream Strand

instance Arbitrary Strand where
  arbitrary = do
    b <- choose (0,3)
    return $ Strand b
  shrink (Strand j)
    | 0<j = [Strand $ j-1]
    | otherwise = []

