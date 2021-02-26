
-- | Different types of energies and conversion between.
--
-- TODO enthalpy
-- TODO entropy

module Biobase.Types.Energy where

import Control.DeepSeq
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data
import Data.Default
import Data.Hashable
import GHC.Real
import Data.Serialize (Serialize)
import Data.Vector.Unboxed.Deriving
import GHC.Generics

import Algebra.Structure.Semiring
import Numeric.Discretized
import Numeric.Limits



-- | Gibbs free energy change.
--
-- For RNA structure, the change in energy from the unfolded structure to
-- the given structure.
--
-- In units of @kcal / mol@.
--
-- TODO shall we phantom-type the actual units?

newtype DG = DG { dG :: Double }
  deriving (Eq,Ord,Num,Fractional,Read,Show,Generic,Data,Typeable)
makeLenses ''DG

derivingUnbox "DG"
  [t| DG -> Double |]  [| dG |]  [| DG |]

instance Hashable  DG
instance Binary    DG
instance Serialize DG
instance FromJSON  DG
instance ToJSON    DG
instance NFData    DG

deriving instance NumericLimits DG
deriving instance NumericEpsilon  DG

instance Default DG where
  def = maxFinite / 100
  {-# Inline def #-}



-- | Discretized @DG@.

newtype DDG = DDG { dDG ∷ Discretized (1 :% 100) }
  deriving (Eq,Ord,Num,Read,Show,Generic,Real,Enum)

ddg2Int :: DDG -> Int
ddg2Int (DDG (Discretized e)) = e

derivingUnbox "DDG"
  [t| DDG -> Int |]  [| getDiscretized . dDG |]  [| DDG . Discretized |]

instance Semiring DDG where
  plus  (DDG x) (DDG y) = DDG $ min x y
  times (DDG x) (DDG y) = DDG $ x `plus` y
  zero = DDG maxFinite
  one  = DDG zero
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}

--instance Hashable  DeltaDekaGibbs
--instance Binary    DeltaDekaGibbs
--instance Serialize DeltaDekaGibbs
--instance FromJSON  DeltaDekaGibbs
--instance ToJSON    DeltaDekaGibbs
--instance NFData    DeltaDekaGibbs
--
--deriving instance NumericLimits DeltaDekaGibbs
--
--instance Default DeltaDekaGibbs where
--  def = maxFinite `div` 100
--  {-# Inline def #-}
--
