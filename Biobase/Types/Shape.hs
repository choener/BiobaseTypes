
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
import           Data.List (foldl1')
import           Data.Monoid ((<>))
import           Data.Set (Set)
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as L
import qualified Data.Set as Set

import           Data.Forest.StructuredPaired

import qualified Biobase.Types.Structure as TS



-- | Shape levels are hardcoded according to their specification.
--
-- TODO Allow compile-time check on accepted shape levels?

data ShapeLevel
  = SL1
  | SL2
  | SL3
  | SL4
  | SL5
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

instance NFData ShapeLevel



-- | The type of RNA shapes. Keeps the type 

data RNAshape
  = RNAshape
    { _rnashapelevel  ∷ !ShapeLevel
    -- ^ The type of shape encoded here.
    , _rnashape       ∷ !ByteString
    -- ^ The actual shape as a string.
    }
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)
makeLenses ''RNAshape

instance NFData RNAshape



-- | Given a compactified 'SPForest', creates a shape forest of the given level.
--
--
--
-- TODO needs newtyping

shapeForest
  ∷ ShapeLevel
  → SPForest ByteString ByteString
  → SPForest Char Char
shapeForest = preStem
  where
    -- | In @preStem@, we aim to close in on the next stem. @SPE@ means that we
    -- reached an end in a stem.
    preStem s SPE = SPE
    -- | The start of a tree structure. The forest is compact, which means that
    -- the element in @xs@ is, by definition, not a continuation of a stack.
    preStem s (SPT _ xs _) = SPT '[' (inStem s xs) ']'
    -- |
    preStem s spr@(SPR rs) = inStem s spr -- = error $ "preStem/SPR " ++ show rs
    -- |
    preStem s (SPJ xs)
      | [x] ← xs  = preStem s x
      -- left bulge
      | [l@SPR{},x@SPT{}] ← xs = if s <= SL2 then (SPJ [SPR '_', preStem s x]) else preStem s x
      -- right bulge
      | [x@SPT{},r@SPR{}] ← xs = if s <= SL2 then (SPJ [preStem s x, SPR '_']) else preStem s x
      | otherwise = SPJ $ map (preStem s) xs -- error $ "preStem/SPJ " ++ show xs
    --
    -- | After a stem, there could be an @SPE@ element.
    inStem s SPE = SPE
    -- | This case happens when eradicating unstructured regions with high
    -- abstraction levels.
    inStem s (SPT _ xs _) = inStem s xs
    inStem s (SPR rs)
      | s == SL1  = SPR '_' -- = error $ "inStem / SPR " ++ show rs
      | otherwise = SPE
    inStem s (SPJ xs)
      | [x] ← xs = error "x"
      -- left bulge
      | [l@SPR{},x] ← xs = if s <= SL3 then preStem s (SPJ xs) else inStem s x
      -- right bulge
      | [x,r@SPR{}] ← xs = if s <= SL3 then preStem s (SPJ xs) else inStem s x
      -- interior loop
      | [l@SPR{},x,r@SPR{}] ← xs = if s == SL5 then inStem s x else preStem s (SPJ xs)
--      | s == SL1  = error $ "inStem / SPJ " ++ show xs
--      | s == SL2  = error $ "inStem / SPJ " ++ show xs
      -- multibranched loop
      | otherwise = SPJ $ map (preStem s) xs

-- | turn into unit test. also reverse of the input should give reverse shape!
-- this then gives a quickcheck test, reversing the input should reverse the shape
--
-- TODO requires generating secondary structures via @Arbitrary@.

test lvl = shapeForestshape . shapeForest lvl $ TS.compactifySPForest $ either error id $ TS.rnassSPForest $ TS.RNAss "(((((...(((..(((...))))))...(((..((.....))..)))))))).."

{-
shapeForest SL5 = go
  where
    go SPE = SPE
    go (SPT _ xs _)
      | SPE ← xs, SPR{} ← xs, [] ← ts = SPT '[' SPE ']'
      | [t] ← ts = go t
      | otherwise = SPT '[' (SPJ $ map go ts) ']'
      where (SPJ ys) = xs
            ts = [ t | t@SPT{} ← ys ]
    -- should only happen on a single unfolded structure
    go (SPR _) = SPR '_'
    go (SPJ xs)
      | [] ← ts   = SPR '_'
      | [t] ← ts  = go t
      | otherwise = SPJ $ map go ts
      where ts = [ t | t@SPT{} ← xs ]
    go xs = error $ show xs ++ " should no be reached"
-}

-- | 

shapeForestshape
  ∷ SPForest Char Char
  → RNAshape
shapeForestshape = RNAshape SL5 . go
  where
    go SPE = ""
    go (SPT l x r) = BS8.singleton l <> go x <> BS8.singleton r
    go (SPJ xs   ) = mconcat $ map go xs
    go (SPR   x  ) = BS8.singleton x -- error "should not be reached" -- BS8.singleton x

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

