
-- | Shape abstractions of structures.
--
-- Shapes do not preserve sizes of structures (say unpaired regions or stem
-- length). As such, distance measures provided here are to be used carefully!
--
-- TODO consider how to handle the different shape levels. One option would be
-- to phantom-type everything.

module Biobase.Types.Shape where

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

import qualified Biobase.Types.Structure as TS



-- | Shape levels are hardcoded according to their specification. Allow
-- compile-time check on accepted shape levels.

data ShapeLevel
  = SL1
  | SL2
  | SL3
  | SL4
  | SL5
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance NFData ShapeLevel



-- |

data RNAshape = RNAshape { _rnashapelevel ∷ ShapeLevel, _rnashape ∷ ByteString }
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)
makeLenses ''RNAshape

instance NFData RNAshape



-- | 

generateShape ∷ ShapeLevel → TS.RNAss → RNAshape
generateShape = undefined


-- * Distance measures on the shape string itself.

-- | Wrapper for string-positional shapes. Intentionally chosen long name.

data RNAshapepset = RNAshapepset { _rnashapepsetlevel ∷ ShapeLevel, _rnashapepset ∷ Set (Int,Int) }
  deriving (Read,Show,Eq,Ord,Generic)
makeLenses ''RNAshapepset

instance NFData RNAshapepset

-- | Transform an 'RNAss' into a set of base pairs @(i,j)@. The pairs are
-- 0-based.

rnashapePairSet
  ∷ (MonadError String m)
  ⇒ RNAshape
  → m RNAshapepset
rnashapePairSet (RNAshape lvl s2) = do
  let go (set,ks  ) (i,'[') = return (set,i:ks)
      go (set,i:is) (j,']') = return (Set.insert (i,j) set, is)
      go (set,[]  ) (j,']') = throwError $ "unequal brackets in \"" ++ BS8.unpack s2 ++ "\" at position: " ++ show j
      go (set,ks  ) (_,'_') = return (set,ks)
  (set,ss) ← foldM go (Set.empty,[]) . L.zip [0..] $ BS8.unpack s2
  unless (null ss) $ throwError $ "unequal brackets in \"" ++ BS8.unpack s2 ++ "\" with opening bracket(s): " ++ show ss
  return $ RNAshapepset lvl set
{-# Inlinable rnashapePairSet #-}

-- | RNA pair set, but a transformation error calls @error@.

rnassPairSet' ∷ RNAshape → RNAshapepset
rnassPairSet' = either error id . rnashapePairSet

-- | Calculates the number of different base pairs betwwen two structures.
--
-- TODO error out on different shape levels

shapePairDist ∷ RNAshapepset → RNAshapepset → Int
shapePairDist (RNAshapepset lvl1 p1) (RNAshapepset lvl2 p2) = Set.size z1 + Set.size z2
  where i = Set.intersection p1 p2
        z1 = p1 `Set.difference` i
        z2 = p2 `Set.difference` i

