{-# LANGUAGE DeriveGeneric        #-}
module Shared.Scene.Actions
  where

import Data.Aeson (FromJSON, fieldLabelModifier, parseJSON, ToJSON(..), defaultOptions, camelTo2, genericParseJSON, genericToJSON)
import           Servant.API (URI(..))
import           Touch
import GHC.Generics (Generic)
import Miso.String (MisoString)
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time

data Action
  = Alert
  | ChangeURI URI
  | HandleURI URI
  | ToggleNavMenu
  | GetBollinger
  | GetRandom
  | GetSma
  | SetRandom QwdeRandom
  | SetSma QwdeSma
  | SetBollinger QwdeBollinger
  | HandleTouch TouchEvent
  | HandleMouse (Int, Int)
  | ParseFromdate MisoString
  | SetFromdate Time.Day
  | Init
  | NoOp
  deriving (Show, Eq)

data QwdeRandom = QwdeRandom {
  numbers :: [Double]
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
instance ToJSON QwdeRandom where
  toJSON = genericToJSON defaultOptions
instance ToJSON QwdeSma where
  toJSON = genericToJSON defaultOptions
instance ToJSON QwdeBollinger where
  toJSON = genericToJSON defaultOptions
