module Main
  where

import Control.Monad
import qualified Test.Data.Graph.Plotter as Plotter
import Test.HUnit
import Test.HUnit.Text
import System.Exit (exitFailure, exitSuccess)

-- | Convenience wrapper for 'runTestTT'.
-- --   Simply runs 'runTestTT' and then exits back to the OS,
-- --   using 'exitSuccess' if there were no errors or failures,
-- --   or 'exitFailure' if there were. For example:
-- --
-- --   > tests :: Test
-- --   > tests = ...
-- --   >
-- --   > main :: IO ()
-- --   > main = runTestTTAndExit tests
--
runTestTTAndExit' :: Test -> IO ()
runTestTTAndExit' tests = do
  c <- runTestTT tests
  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure

main :: IO ()
main = do
  -- runTest[T]ext(to)[T]erminal
  runTestTTAndExit' $ TestList [
    Plotter.tests
    ]
