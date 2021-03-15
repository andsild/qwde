module Main
  where

import Control.Monad
import qualified Test.Shared.Util.Date as TDate
import Test.HUnit
import Test.HUnit.Text
import System.Exit (exitFailure)

main :: IO ()
main = do
  -- runTest[T]ext(to)[T]erminal
  runTestTTAndExit $ TestList [
    TDate.tests
    ]
