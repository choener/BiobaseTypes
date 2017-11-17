
-- | Wrappers for structural data. Encoded as bytestrings. This differs from
-- @BiobaseXNA@, where specialized encodings are used. These structures are
-- supposedly "short", they need to fit into a strict bytestring.
--
-- TODO Consider where to move each type. There are merge possibilities between
-- BiobaseXNA and BiobaseTypes.

module Biobase.Types.Structure where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Error.Class
import           Control.Monad (foldM,unless)
import           Data.ByteString (ByteString)
import           Data.Data
import           Data.Set (Set)
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as L
import qualified Data.Set as Set



-- | Secondary structure using @()@ for paired elements, and @.@ for unpaired
-- ones. It is assumed that the @()@ match up. These structures from a Monoid.

newtype RNAss = RNAss { _rnass ∷ ByteString }
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic,Monoid)
makeLenses ''RNAss

instance NFData RNAss

-- | Ensemble structure encoding. *Very* different type ctor name chosen! The
-- structure of this string makes verification much more complicated.
--
-- TODO describe encoding used by RNAfold for the ensemble string.

newtype RNAensembleStructure = RNAes { _rnaes ∷ ByteString }
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)
makeLenses ''RNAensembleStructure

instance NFData RNAensembleStructure

-- | Cofolded structure.

data RNAds = RNAds
  { _rnadsL ∷ !ByteString
  , _rnadsR ∷ !ByteString
  }
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)
makeLenses ''RNAds

instance NFData RNAds

-- | A Prism that turns ByteStrings with a single @&@ into @RNAds@.

rnads ∷ Prism' ByteString RNAds
rnads = prism (\(RNAds l r) → BS8.concat [l, "&", r])
              (\s → case BS8.split '&' s of [l,r] → Right (RNAds l r) ; _ → Left s)
{-# Inline rnads #-}

-- | Isomorphism from @RNAds@ to @(RNAss,RNAss)@. The @RNAss@ are only
-- legal if taken both: @rnassFromDimer . both@.

rnads2rnassPair ∷ Iso' RNAds (RNAss, RNAss)
rnads2rnassPair = iso (\(RNAds l r) → (RNAss l, RNAss r)) (\(RNAss l, RNAss r) → RNAds l r)
{-# Inline rnads2rnassPair #-}

-- | Try to create a dimeric structure.

mkRNAds ∷ (Monad m, MonadError RNAStructureError m) ⇒ ByteString → m RNAds
mkRNAds q = BS8.split '&' q & \case
    [l,r] → do
      -- TODO can still fail with unmatched brackets.
      return $ RNAds
        { _rnadsL = l
        , _rnadsR = r
        }
    _     → throwError $ RNAStructureError "mkRNAds: not a dimer" q
{-# Inline mkRNAds #-}

-- | Capture what might be wrong with the RNAss.

data RNAStructureError = RNAStructureError
  { _rnaStructureError  ∷ String
  , _rnaOffender        ∷ ByteString
  }
  deriving (Show,Generic)

instance NFData RNAStructureError

-- | Verifies that the given RNAss is properly formatted. Otherwise, error out.
--
-- TODO Implement! Check with BiobaseXNA and the stack effort in there. This
-- might influence if the verification goes into BiobaseXNA and happens via an
-- @Iso'@.

verifyRNAss ∷ (Monad m, MonadError RNAStructureError m) ⇒ RNAss → m RNAss
verifyRNAss ss = do
  return ss

newtype RNApset = RNApset { _rnapset ∷ Set (Int,Int) }
  deriving (Read,Show,Eq,Ord,Generic)
makeLenses ''RNApset

instance NFData RNApset

-- | Transform an 'RNAss' into a set of base pairs @(i,j)@. The pairs are
-- 0-based.

rnassPairSet
  ∷ (MonadError String m)
  ⇒ RNAss
  → m RNApset
rnassPairSet (RNAss s2) = do
  let go (set,ks  ) (i,'(') = return (set,i:ks)
      go (set,i:is) (j,')') = return (Set.insert (i,j) set, is)
      go (set,[]  ) (j,')') = throwError $ "unequal brackets in \"" ++ BS8.unpack s2 ++ "\" at position: " ++ show j
      go (set,ks  ) (_,'.') = return (set,ks)
  (set,ss) ← foldM go (Set.empty,[]) . L.zip [0..] $ BS8.unpack s2
  unless (null ss) $ throwError $ "unequal brackets in \"" ++ BS8.unpack s2 ++ "\" with opening bracket(s): " ++ show ss
  return $ RNApset set
{-# Inlinable rnassPairSet #-}

pairDist ∷ RNApset → RNApset → Int
pairDist (RNApset p1) (RNApset p2) = Set.size z1 + Set.size z2
  where i = Set.intersection p1 p2
        z1 = p1 `Set.difference` i
        z2 = p2 `Set.difference` i

