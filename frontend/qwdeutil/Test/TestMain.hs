module TestMain
  where

import Control.Monad
import qualified Test.Data.Graph.Plotter as Plotter
import Test.HUnit
import Test.HUnit.Text
import System.Exit (exitFailure)

main :: IO ()
main = do
  -- runTest[T]ext(to)[T]erminal
  runTestTTAndExit $ TestList [
    Plotter.tests
    ]
