{-# LANGUAGE DeriveGeneric        #-}
module Shared.Scene.Model where

import           Servant.API (URI(..))
import qualified Data.Graph.Plotter as P
import Shared.Util.Constants
import Network.URI (parseURI)

data Model = Model {
  uri :: URI
  , navMenuOpen :: Bool
  , mouseCords :: (Int, Int)
  , randomPlot :: P.Plot
  , smaPlot :: P.Plot
  , bollingerPlot :: P.Plot
  --, bollingerFromdateWidget :: View Action -- relies on JS type
  --, bollingerFromdate :: Time.Day
  } deriving (Eq, Show)

initialModel :: Model
initialModel = Model uri False (0,0) (P.getPlot 10 plotWidth plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" defaultColor])
  (P.getPlot 10 plotWidth plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" defaultColor])
  (P.getPlot 10 plotWidth plotHeight (map show ([1..10] :: [Int])) [[1..10]] [P.PlotLegend "" defaultColor])
  where
    uri = case parseURI "http://qwde.no" of
            Just n -> n
            Nothing -> error "misunderstood API?"
