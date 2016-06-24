
module Main where

import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH

import Biobase.Types.NumericalExtremes



main :: IO ()
main = $(defaultMainGenerator)

