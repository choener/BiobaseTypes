
module Main where

import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH

import           Biobase.Types.Bitscore
import           Biobase.Types.NumericalExtremes



-- * Bitscore conversions

prop_ProbScore (Positive null) (Positive x) = x ~= score2Prob null (prob2Score null x)

--prop_ScoreProb (Positive null) x = Bitscore x ~= prob2Score null (score2Prob null $ Bitscore x)



-- * shape properties

-- ** unit tests for known rna secondary structures

-- ** quickcheck

-- | reversing a secondary structure means reversing the shape



-- * generic stuff

a ~= b = abs (b-a) <= 10e-6

main :: IO ()
main = $(defaultMainGenerator)

