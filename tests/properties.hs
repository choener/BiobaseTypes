
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

prop_ScoreProb (Positive null) x = Bitscore x ~= prob2Score null (score2Prob null $ Bitscore x)


a ~= b = abs (b-a) <= 10e-6

main :: IO ()
main = $(defaultMainGenerator)

