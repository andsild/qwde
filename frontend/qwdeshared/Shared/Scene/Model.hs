{-# LANGUAGE DeriveGeneric        #-}
module Shared.Scene.Model where

import           Servant.API (URI(..))
import qualified Data.Graph.Plotter as P
import Shared.Util.Constants
import Network.URI (parseURI)
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import           Data.Time.LocalTime ( LocalTime(..), getCurrentTimeZone, utcToLocalTime )

data Model = Model {
  uri :: URI
  , navMenuOpen :: Bool
  , mouseCords :: (Int, Int)
  , randomPlot :: P.Plot
  , smaPlot :: P.Plot
  , bollingerPlot :: P.Plot
  , fromDate :: Time.Day
  --, bollingerFromdateWidget :: View Action -- relies on JS type
  --, bollingerFromdate :: Time.Day
  } deriving (Eq, Show)

initialModel :: IO Model
initialModel = do
  curTime <- Time.getCurrentTime
  timeZone <- getCurrentTimeZone
  let day :: Time.Day
      (LocalTime day _timeOfDay) = utcToLocalTime timeZone curTime

  pure $ Model uri False (0,0) (P.getPlot 10 plotWidth plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" defaultColor])
    (P.getPlot 10 plotWidth plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" defaultColor])
    (P.getPlot 10 plotWidth plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" defaultColor])
    day
    where
      uri = case parseURI "http://qwde.no" of
              Just n -> n
              Nothing -> error "misunderstood API?"
