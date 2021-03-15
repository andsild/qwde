{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Shared.Util.Date
  (tests)
  where

import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time

import Shared.Util.Date
import           Test.HUnit (Test (..), assertEqual)

testParse :: Test
testParse = TestCase $ do
  let inData = "2021-03-01" :: String
  out <- parseStringDate inData
  let expected = Time.fromGregorian 2021 3 1
    in
      assertEqual "date check" expected out

testToString :: Test
testToString = TestCase $ do
  let inData = Time.fromGregorian 2021 3 1
      out = dateToString inData
      expected = "20210301"
    in
      assertEqual "date check" expected out


tests :: Test
tests = TestList [ testParse
  , testToString
  ]
