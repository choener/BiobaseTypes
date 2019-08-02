
module Main where

import           Control.Lens
import           Debug.Trace
import qualified Data.ByteString.Char8 as BS8
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH

import           Biobase.Types.BioSequence
import           Biobase.Types.Bitscore
import           Biobase.Types.Location
import           Biobase.Types.Shape
import           Biobase.Types.Strand
import           Biobase.Types.Structure



-- * Bitscore conversions

prop_ProbScore (Positive null) (Positive x) = x ~= score2Prob null (prob2Score null x)

--prop_ScoreProb (Positive null) x = Bitscore x ~= prob2Score null (score2Prob null $ Bitscore x)



-- * sequence properties

-- complement twice

prop_complement_twice_DNA (dna ∷ BioSequence DNA) = dna == dna^.complement.complement

prop_complement_twice_RNA (rna ∷ BioSequence RNA) = rna == rna^.complement.complement

prop_transcribe_twice_DNA (dna ∷ BioSequence DNA) = dna == dna^.transcribe.transcribe

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

-- | Given a BioSequenceWindow, and different takes and drops, check wether what we have corresponds to what we want

case_bswTakeDrop ∷ Assertion
case_bswTakeDrop = do
  let w = BioSequenceWindow @"DNA" @DNA "test" 1 "ACGTAC" 3 (FwdLocation PlusStrand 0 6)
  bswTake 0 w @?= BioSequenceWindow "test" 0 "" 0 (FwdLocation PlusStrand 0 0)
  bswTake 1 w @?= BioSequenceWindow "test" 1 "A" 0 (FwdLocation PlusStrand 0 1)
  bswTake 2 w @?= BioSequenceWindow "test" 1 "AC" 0 (FwdLocation PlusStrand 0 2)
  bswTake 6 w @?= BioSequenceWindow "test" 1 "ACGTAC" 3 (FwdLocation PlusStrand 0 6)
  --
  bswDrop 0 w @?= BioSequenceWindow "test" 1 "ACGTAC" 3 (FwdLocation PlusStrand 0 6)
  bswDrop 1 w @?= BioSequenceWindow "test" 0 "CGTAC" 3 (FwdLocation PlusStrand 1 5)
  bswDrop 6 w @?= BioSequenceWindow "test" 0 "" 0 (FwdLocation PlusStrand 6 0)


-- * generic stuff

a ~= b = abs (b-a) <= 10e-6

main :: IO ()
main = $(defaultMainGenerator)

