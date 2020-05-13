module Shared.Util.Constants where

import Miso.String
import Data.Colour.Names (black)
import Data.Colour (Colour)

githubUrl :: MisoString
githubUrl = toMisoString "https://github.com/andsild/qwde"

misoSrc :: MisoString
misoSrc = toMisoString "/static/Creative-Tail-Animal-cat.svg"

defaultColor :: Colour Double
defaultColor = black

plotWidth :: Int
plotWidth = 800
plotHeight :: Int
plotHeight = 500
numLabels :: Int
numLabels = 10

