
-- | Different types of energies and conversion between.
--
-- TODO enthalpy
-- TODO entropy

module Biobase.Types.Energy where

import Control.DeepSeq
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default
import Data.Hashable
import Data.Serialize (Serialize)
import Data.Vector.Unboxed.Deriving
import GHC.Generics

import Biobase.Types.NumericalExtremes



-- | Gibbs free energy change.
--
-- For RNA structure, the change in energy from the unfolded structure to
-- the given structure.
--
-- In units of @kcal / mol@.
--
-- TODO shall we phantom-type the actual units?

newtype DeltaGibbs = DG { getDG :: Double }
  deriving (Eq,Ord,Num,Fractional,Read,Show,Generic)



derivingUnbox "DeltaGibbs"
  [t| DeltaGibbs -> Double |]  [| getDG |]  [| DG |]

instance Hashable  DeltaGibbs
instance Binary    DeltaGibbs
instance Serialize DeltaGibbs
instance FromJSON  DeltaGibbs
instance ToJSON    DeltaGibbs
instance NFData    DeltaGibbs

deriving instance NumericalExtremes DeltaGibbs
deriving instance NumericalEpsilon  DeltaGibbs

instance Default DeltaGibbs where
  def = extremelyLarge
  {-# Inline def #-}



-- | @round $ DeltaGibbs / 100@.

newtype DeltaDekaGibbs = DekaG { getDekaG :: Int }
  deriving (Eq,Ord,Num,Read,Show,Generic)



derivingUnbox "DeltaDekaGibbs"
  [t| DeltaDekaGibbs -> Int |]  [| getDekaG |]  [| DekaG |]

instance Hashable  DeltaDekaGibbs
instance Binary    DeltaDekaGibbs
instance Serialize DeltaDekaGibbs
instance FromJSON  DeltaDekaGibbs
instance ToJSON    DeltaDekaGibbs
instance NFData    DeltaDekaGibbs

deriving instance NumericalExtremes DeltaDekaGibbs

instance Default DeltaDekaGibbs where
  def = extremelyLarge
  {-# Inline def #-}

