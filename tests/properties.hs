
module Main where

import           Control.Lens
import           Debug.Trace
import qualified Data.ByteString.Char8 as BS8
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH

import           Biobase.Types.Bitscore
import           Biobase.Types.NumericalExtremes
import           Biobase.Types.Sequence
import           Biobase.Types.Shape
import           Biobase.Types.Structure



-- * Bitscore conversions

prop_ProbScore (Positive null) (Positive x) = x ~= score2Prob null (prob2Score null x)

--prop_ScoreProb (Positive null) x = Bitscore x ~= prob2Score null (score2Prob null $ Bitscore x)



-- * sequence properties

-- complement twice

prop_complement_twice_DNA (dna ∷ DNAseq) = dna == dna^.complement.complement

prop_complement_twice_RNA (rna ∷ RNAseq) = rna == rna^.complement.complement

prop_transcribe_twice_DNA (dna ∷ DNAseq) = dna == dna^.transcribe.transcribe

--prop_transcribe_twice_DNA (rna ∷ RNAseq) = rna == rna^.transcribe.transcribe

-- * shape properties

-- ** unit tests for known rna secondary structures

-- ** quickcheck

-- | reversing a secondary structure means reversing the shape

prop_StructureShape_5_Reverse = fun_StructureShape_k_Reverse SL5
prop_StructureShape_4_Reverse = fun_StructureShape_k_Reverse SL4
prop_StructureShape_3_Reverse = fun_StructureShape_k_Reverse SL3
prop_StructureShape_2_Reverse = fun_StructureShape_k_Reverse SL2
prop_StructureShape_1_Reverse = fun_StructureShape_k_Reverse SL1

fun_StructureShape_k_Reverse lvl rnass@(RNAss s2)
  | shp == fshp = True
  | otherwise = traceShow (s2,shp,rshp,fshp) False
  where shp  = rnass2shape lvl rnass
        rshp = rnass2shape lvl $ RNAss $ BS8.map flp $ BS8.reverse s2
        fshp = over rnashape (BS8.map flp . BS8.reverse) rshp
        flp '(' = ')'
        flp ')' = '('
        flp '[' = ']'
        flp ']' = '['
        flp x   = x



-- * generic stuff

a ~= b = abs (b-a) <= 10e-6

main :: IO ()
main = $(defaultMainGenerator)

