
-- | Bit scores as used by different algorithms in bioinformatics,
-- linguistics, and probably elsewhere.
--
-- Basically, the base-2 logarithm of the probability of the input given
-- the model vs the probability of the input given the null model.
--
-- @
-- S = log_2 (P(seq|model) / P(seq|null))
-- @
--

module Biobase.Types.Bitscore where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.Default
import           Data.Hashable (Hashable)
import           Data.Primitive.Types
import           Data.Serialize
import           Data.Vector.Unboxed.Base
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics (Generic)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import           Algebra.Structure.Semiring
import           Numeric.Limits



-- | Bit score; behaves like a double (deriving Num). In particular, the
-- algebraic operations behave as expected @Bitscore a + Bitscore b ==
-- Bitscore (a+b)@.
--
-- Currently geared towards use as in @Infernal@ and @HMMER@.
--
-- Infernal users guide, p.42: log-odds score in log_2 (aka bits).

newtype Bitscore = Bitscore { getBitscore :: Double }
  deriving stock (Eq,Ord,Read,Show,Generic)
  deriving newtype (Num,Fractional)

instance Semiring Bitscore where
  plus = (+)
  times = (*)
  zero = 0
  one = 1
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}

instance Binary    Bitscore
instance FromJSON  Bitscore
instance Hashable  Bitscore
instance Serialize Bitscore
instance ToJSON    Bitscore
instance NFData    Bitscore

deriving newtype instance NumericLimits Bitscore

derivingUnbox "Bitscore"
  [t| Bitscore -> Double |] [| getBitscore |] [| Bitscore |]

-- | A default bitscore of "-infinity", but with @10-1@ wiggle room.
--
-- TODO Check out the different "defaults" Infernal uses

instance Default Bitscore where
  def = Bitscore minFinite / 100
  {-# Inline def #-}

-- | Given a null model and a probability, calculate the corresponding
-- 'BitScore'.
--
-- TODO @x<=epsilon@ ?

prob2Score :: Double -> Double -> Bitscore
prob2Score null x
  | x==0      = minFinite / 100
  | otherwise = Bitscore $ log (x/null) / log 2
{-# Inline prob2Score #-}

-- | Given a null model and a 'BitScore' return the corresponding probability.

score2Prob :: Double -> Bitscore -> Double
score2Prob null (Bitscore x)
  | x <= minFinite / 100 = 0
  | otherwise     = null * exp (x * log 2)
{-# Inline score2Prob #-}

