
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

newtype DiscretizedLogOdds = DLO { getDLO :: Int }
  deriving (Generic,Eq,Ord,Show,Read,Num)

derivingUnbox "DiscretizedLogOdds"
  [t| DiscretizedLogOdds -> Int |]  [| getDLO |]  [| DLO |]

instance Binary    DiscretizedLogOdds
instance Serialize DiscretizedLogOdds
instance FromJSON  DiscretizedLogOdds
instance ToJSON    DiscretizedLogOdds
instance Hashable  DiscretizedLogOdds

instance NFData DiscretizedLogOdds where
  rnf (DLO k) = rnf k
  {-# Inline rnf #-}

