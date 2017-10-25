
-- | Wrappers for structural data. Encoded as bytestrings. This differs from
-- @BiobaseXNA@, where specialized encodings are used. These structures are
-- supposedly "short", they need to fit into a strict bytestring.
--
-- TODO Consider where to move each type. There are merge possibilities between
-- BiobaseXNA and BiobaseTypes.

module Biobase.Types.Structure where

import           Control.Lens
import           Control.Monad.Error.Class
import           Data.ByteString (ByteString)
import           Data.Data
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS8



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

-- | Cofolded structure.

data RNAssDimer = RNAssDimer
  { _rnassDimer     ∷ !ByteString
  , _rnassDimerPos  ∷ !Int
  }
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)
makeLenses ''RNAssDimer

-- -- | Only the left part of the dimer. Does *not* yield @RNAss@ since the
-- -- pairings might very well span the separating symbol.
-- 
-- rnassDimerL ∷ Iso' RNAssDimer ByteString
-- rnassDimerL = iso (\(RNAssDimer d p) → BS8.take p d) (

-- rnassDimerR ∷ Iso' RNAssDimer → ByteString

-- | Try to create a dimeric structure.

mkRNAssDimer ∷ (Monad m, MonadError RNAStructureError m) ⇒ ByteString → m RNAssDimer
mkRNAssDimer q = BS8.findIndex (=='&') q & \case
    Nothing  → throwError $ RNAStructureError "mkRNAssDimer: not a dimer" q
    Just pos → do
      -- TODO can still fail with unmatched brackets.
      return $ RNAssDimer
        { _rnassDimer     = q
        , _rnassDimerPos  = pos
        }

-- | Capture what might be wrong with the RNAss.

data RNAStructureError = RNAStructureError
  { _rnaStructureError  ∷ String
  , _rnaOffender        ∷ ByteString
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

