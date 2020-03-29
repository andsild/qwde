{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings          #-}
module Manifest where

import           Data.Aeson (ToJSON)
import           Data.Text                            (Text)
import           GHC.Generics (Generic(..))

data Manifest
  = Manifest
  { name :: Text
  , short_name :: Text
  , start_url :: Text
  , display :: Text
  , theme_color :: Text
  , description :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Manifest

misoManifest :: Manifest
misoManifest =
  Manifest { name = "QWDE. Work in progress"
           , short_name = "qwde"
           , start_url = "."
           , display = "standalone"
           , theme_color = "#00d1b2"
           , description = "A WIP"
           }


