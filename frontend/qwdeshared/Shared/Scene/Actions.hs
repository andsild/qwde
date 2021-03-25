{-# LANGUAGE DeriveGeneric        #-}
module Shared.Scene.Actions
  where

import Data.Aeson (FromJSON, parseJSON, ToJSON(..), defaultOptions, genericParseJSON, genericToJSON)
import Servant.API (URI(..))
import GHC.Generics (Generic)
import Miso.String (MisoString)
import qualified Data.Time.Calendar as Time

data Action
  = Alert
  | ChangeURI URI
  | HandleURI URI
  | ToggleNavMenu
  | GetBollinger
  | GetRandom
  | GetTickers
  | GetSma
  | SetRandom QwdeRandom
  | SetSma QwdeSma
  | SetTickers QwdeTickers
  | SetBollinger QwdeBollinger
  | ParseFromdate MisoString
  | SetFromdate Time.Day
  | ParseTodate MisoString
  | SetTodate Time.Day
  | ParseSingleTicker MisoString
  | SetSingleTicker String
  | Init
  | NoOp
  deriving (Show, Eq)

data QwdeRandom = QwdeRandom {
  numbers :: [Double]
} deriving (Eq, Show, Generic)

data QwdeTickers = QwdeTickers {
  tickers :: [String]
} deriving (Eq, Show, Generic)

data QwdeSma = QwdeSma {
  prices :: [Double]
  , sma :: [[Double]]
} deriving (Eq, Show, Generic)

data QwdeBollinger = QwdeBollinger {
  lowerBand :: [Double]
  , mean :: [Double]
  , highBand :: [Double]
  , price :: [Double]
} deriving (Eq, Show, Generic)

instance FromJSON QwdeRandom where
  parseJSON = genericParseJSON defaultOptions
instance FromJSON QwdeSma where
  parseJSON = genericParseJSON defaultOptions
instance FromJSON QwdeBollinger where
  parseJSON = genericParseJSON defaultOptions
instance FromJSON QwdeTickers where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON QwdeRandom where
  toJSON = genericToJSON defaultOptions
instance ToJSON QwdeTickers where
  toJSON = genericToJSON defaultOptions
instance ToJSON QwdeSma where
  toJSON = genericToJSON defaultOptions
instance ToJSON QwdeBollinger where
  toJSON = genericToJSON defaultOptions
