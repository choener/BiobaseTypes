
-- | Wrappers for structural data. Encoded as bytestrings. This differs from
-- @BiobaseXNA@, where specialized encodings are used. These structures are
-- supposedly "short", they need to fit into a strict bytestring.
--
-- TODO Consider where to move each type. There are merge possibilities between
-- BiobaseXNA and BiobaseTypes.

module Biobase.Types.Structure where

import Control.Lens
import Control.Monad.Error.Class
import Data.ByteString (ByteString)
import Data.Data
import GHC.Generics (Generic)



-- | Secondary structure using @()@ for paired elements, and @.@ for unpaired
-- ones. It is assumed that the @()@ match up.

newtype RNAss = RNAss { _rnass ∷ ByteString }
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)
makeLenses ''RNAss

-- | Ensemble structure encoding. *Very* different type ctor name chosen! The
-- structure of this string makes verification much more complicated.
--
-- TODO describe encoding used by RNAfold for the ensemble string.

newtype RNAensembleStructure = RNAes { _rnaes ∷ ByteString }
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)
makeLenses ''RNAensembleStructure

-- | Capture what might be wrong with the RNAss.

data RNAStructureError = RNAStructureError
  { _rnaStructureError  ∷ String
  , _rnaSSError         ∷ RNAss
  }
  deriving (Show)

-- | Verifies that the given RNAss is properly formatted. Otherwise, error out.
--
-- TODO Implement! Check with BiobaseXNA and the stack effort in there. This
-- might influence if the verification goes into BiobaseXNA and happens via an
-- @Iso'@.

verifyRNAss ∷ (Monad m, MonadError RNAStructureError m) ⇒ RNAss → m RNAss
verifyRNAss ss = do
  return ss

