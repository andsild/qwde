{-# LANGUAGE DeriveGeneric        #-}
module Shared.Scene.Model where

import           Servant.API (URI(..))
import qualified Data.Graph.Plotter as P
import Shared.Util.Constants
import Network.URI (parseURI)
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
import qualified Data.Time.Clock as Time
import           Data.Time.LocalTime ( LocalTime(..), getCurrentTimeZone, utcToLocalTime )
import qualified Shared.Scene.Actions as Actions

data Model = Model {
  uri :: URI
  , navMenuOpen :: Bool
  , mouseCords :: (Int, Int)
  , randomPlot :: P.Plot
  , smaPlot :: P.Plot
  , bollingerPlot :: P.Plot
  , fromDate :: Time.Day
  , toDate :: Time.Day
  , ticks :: [String]
  , singleTicker :: String
  , dataLoaded :: Bool
  } deriving (Eq, Show)

initialModel :: IO Model
initialModel = do
  curTime <- Time.getCurrentTime
  timeZone <- getCurrentTimeZone
  let fromDay :: Time.Day
      (LocalTime fromDay _fromTimeOfDay) = utcToLocalTime timeZone curTime
      toDay :: Time.Day
      (LocalTime toDay _toTimeOfDay) = utcToLocalTime timeZone curTime

  pure $ Model uri False (0,0) (P.getPlot 10 plotWidth plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" defaultColor])
    (P.getPlot 10 plotWidth plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" defaultColor])
    (P.getPlot 10 plotWidth plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" defaultColor])
    fromDay toDay
    []
    "twtr"
    False
    where
      uri = case parseURI "http://qwde.no" of
              Just n -> n
              Nothing -> error "misunderstood API?"
