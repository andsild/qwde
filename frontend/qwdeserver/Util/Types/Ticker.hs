{-# LANGUAGE DeriveGeneric             #-}
module Util.Types.Ticker (
  Ticker,
  mkUpper
  ) where

import Data.Aeson (FromJSON)
import Data.Char (toUpper)
import           GHC.Generics (Generic(..))
import Servant.API (ToHttpApiData, ToHttpApiData(..), FromHttpApiData(..))
import qualified Data.Text                    as T

newtype Ticker = Ticker String deriving (Eq, Read, Ord, Generic)
instance FromJSON Ticker
instance ToHttpApiData Ticker where
  toUrlPiece s = T.pack (show s)
  toQueryParam s = T.pack (show s)

instance FromHttpApiData Ticker where
  parseUrlPiece = Right . mkUpper . show

instance Show Ticker where
  show (Ticker s) = s

mkUpper :: String -> Ticker
mkUpper s = Ticker $ map toUpper s
