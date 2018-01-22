
-- | Wrappers for structural data. Encoded as bytestrings. This differs from
-- @BiobaseXNA@, where specialized encodings are used. These structures are
-- supposedly "short", they need to fit into a strict bytestring.
--
-- TODO Consider where to move each type. There are merge possibilities between
-- BiobaseXNA and BiobaseTypes.
--
-- TODO QuickCheck @Arbitrary@ for @RNAss@.

module Biobase.Types.Structure where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Error.Class
import           Control.Monad (foldM,unless)
import           Data.Attoparsec.ByteString.Char8
import           Data.Attoparsec.Combinator
import           Data.Bifunctor (second)
import           Data.ByteString (ByteString)
import           Data.Data
import           Data.List (foldl1',foldl')
import           Data.Monoid ((<>))
import           Data.Set (Set)
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Set as Set
import qualified Test.QuickCheck as Q

import           Data.Forest.StructuredPaired



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

-- | The set of nucleotide pairs, together with the sequence length.

data RNApset = RNApset
  { _rnapset      ∷ !(Set (Int,Int))
    -- ^ the set of nucleotide pairs.
  , _rnapsetSLen  ∷ !Int
    -- ^ length of the underlying nucleotide sequence.
  }
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
  return $ RNApset set (BS8.length s2)
{-# Inlinable rnassPairSet #-}

-- | Genereate a simple structured/paired forest from a secondary structure string.

rnassSPForest
  ∷ (MonadError String m)
  ⇒ RNAss
  → m (SPForest ByteString Char)
rnassSPForest (RNAss s2) = either throwError return $ parseOnly (manyElems <* endOfInput) s2
  where
    tree = SPT <$> char '(' <*> someElems <*> char ')' <?> "SPT"
    unpaired  = SPR <$> takeWhile1 (=='.') <?> "SPR"
    someElems = SPJ <$> many1 (tree <|> unpaired) <?> "many1 SPT / SPR"
    manyElems = (\case {[] → SPE; xs → SPJ xs}) <$> many  (tree <|> unpaired) <?> "many0 SPT / SPR"
{-# Inlinable rnassSPForest #-}

-- | Compactify such an SPForest. This means that all stems are now represented
-- by a single 'SPT' data constructor.

compactifySPForest
  ∷ SPForest ByteString Char
  → SPForest ByteString ByteString
compactifySPForest = go . second BS8.singleton
  where go SPE      = SPE
        go (SPR x)  = SPR x
        go (SPJ xs) = SPJ (map go xs)
        go (SPT l (SPJ [x]) r) = go $ SPT l x r
        go (SPT l (SPT l' t r') r) = go $ SPT (l <> l') t (r' <> r)
        go (SPT l t             r) = SPT l (go t) r

-- | RNA pair set, but a transformation error calls @error@.

rnassPairSet' ∷ RNAss → RNApset
rnassPairSet' = either error id . rnassPairSet

-- | Calculates the number of different base pairs between two structures. This
-- ignores the length of the underlying sequences.

pairDist ∷ RNApset → RNApset → Int
pairDist (RNApset p1 _) (RNApset p2 _) = Set.size z1 + Set.size z2
  where i = Set.intersection p1 p2
        z1 = p1 `Set.difference` i
        z2 = p2 `Set.difference` i



-- * Arbitrary instances. This only creates legal instances, but does *not*
-- take into account ViennaRNA rules like three unpaired nucleotides in the
-- hairpin.
--
-- TODO @shrink@ is a bit more complicated, but can be done via a set of pairs.

instance Q.Arbitrary RNApset where
  arbitrary = do
    -- generate RNA structures between 0 and 100 nucleotides.
    l ∷ Int ← Q.choose (0,100)
    -- Given left and right bounds, create pairs.
    let go ∷ Int → Int → Q.Gen (Set (Int,Int))
        go l r
          | l >= r    = return S.empty
          | otherwise = do
            stack ← undefined
            return undefined
    return undefined

