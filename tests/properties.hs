
module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property

import Biobase.Types.NumericalExtremes



main :: IO ()
main = $(defaultMainGenerator)

