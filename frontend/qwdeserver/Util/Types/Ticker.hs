module Util.Types.Ticker (
  Ticker,
  mkUpper
  ) where

import Data.Char (toUpper)
import Data.Maybe (fromJust, Maybe(..))
import Servant.API (ToHttpApiData, ToHttpApiData(..), FromHttpApiData(..))
import qualified Data.Text                    as T

newtype Ticker = Ticker String deriving (Eq, Read, Ord)
instance ToHttpApiData Ticker where
  toUrlPiece s = T.pack (show s)
  toQueryParam s = T.pack (show s)

instance FromHttpApiData Ticker where
  parseUrlPiece = Right . mkUpper . show
  --parseQueryParam s = T.pack (show s)

instance Show Ticker where
  show (Ticker s) = s

mkUpper :: String -> Ticker
mkUpper s = Ticker $ map toUpper s
