
-- | Discretized log-odds.

module Biobase.Types.Odds where

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON,ToJSON)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)



-- | Discretized log-odds.
--
-- The BLOSUM matrices, for example, store data in discretized log-odds
-- form.
--
-- TODO Might move up even higher into statistics modules.

newtype DLO = DLO { getDLO :: Int }
  deriving (Generic,Eq,Ord,Show,Read)

derivingUnbox "DLO"
  [t| DLO -> Int |]  [| getDLO |]  [| DLO |]

instance Binary    DLO
instance Serialize DLO
instance FromJSON  DLO
instance ToJSON    DLO
instance Hashable  DLO

instance NFData DLO where
  rnf (DLO k) = rnf k
  {-# Inline rnf #-}

