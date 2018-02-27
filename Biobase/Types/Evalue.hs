
-- | Encode the number of hits to expect. This is typically dependent on
-- some "database size". Evalues are bounded by @[0,infinity)@.
--
-- TODO Evalues close to zero are more interesting. We should strongly
-- consider log-conversion here.

module Biobase.Types.Evalue where

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

import           Numeric.Limits



-- | Type-safe wrapper for e-values.

newtype Evalue = Evalue { getEvalue :: Double }
  deriving (Eq,Ord,Read,Show,Num,Generic)

instance Binary    Evalue
instance FromJSON  Evalue
instance Hashable  Evalue
instance Serialize Evalue
instance ToJSON    Evalue
instance NFData    Evalue

derivingUnbox "Evalue"
  [t| Evalue -> Double |] [| getEvalue |] [| Evalue |]

-- | By default, we expect no hits.

instance Default Evalue where
  def = Evalue 0
  {-# Inline def #-}

instance NumericLimits Evalue where
  maxFinite   = Evalue maxFinite
  minFinite   = Evalue 0
  {-# Inline maxFinite  #-}
  {-# Inline minFinite  #-}

