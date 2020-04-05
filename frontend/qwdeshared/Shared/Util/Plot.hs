{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Shared.Util.Plot where

import qualified Data.Graph.Plotter as P
import Shared.Scene.Actions
import Miso
import Miso.Html.Element (input_)
import qualified Miso.Svg as SVG
import Data.Colour.SRGB (sRGB24show)
import qualified Miso.Svg.Attribute as SVGA
import Miso.String hiding (map, zip)
import qualified Data.Map as M
import Data.Colour (Colour)

drawPlot :: P.Plot -> [Char] -> Action -> View Action
drawPlot plot name action = div_ [ class_  "content has-text-centered" ] ([
                input_ []
                 , div_ [ id_ . toMisoString $ (name ++ "id") ] [
                    SVG.svg_ [ class_ "graph", SVGA.visibility_ showGraph] ([
                         makeAxis True (P.xAxis plot)
                         , makeAxis False (P.yAxis plot)
                         , makeLabelpoints True (P.xAxis plot)
                         , makeLabelpoints False (P.yAxis plot)
                       ] ++ (map (\(p,l) -> makeLine (pairs $ P.xTicks p) (pairs $ P.yTicks p) (P.color l)) (zip (P.plotData plot) (P.legend plot))))
                   , button_ [ id_ (toMisoString $ name ++ "btn"), onClick action ] [ text "Render plot" ]
                   , makeLegend (P.legend plot) (toMisoString (name ++ "Legend" :: String))
                ]])
  where
    showGraph = (if (Prelude.null $ P.plotData plot) then "hidden" else "visible" )

makeAxis :: Bool -> P.Axis -> View Action
makeAxis isX P.Axis{..} = let letter = if isX then "x" else "y"
  in SVG.g_ [ class_ (toMisoString ("grid " ++ letter ++ "-grid")) ] [
    SVG.line_ [ SVGA.x1_ $ ms x1
              , SVGA.x2_ $ ms x2
              , SVGA.y1_ $ ms y1
              , SVGA.y2_ $ ms y2
    ] [] ]

makeLabelpoints :: Bool -> P.Axis -> View Action
makeLabelpoints isX P.Axis{..} = let letter = if isX then "x" else "y"
  in SVG.g_ [ class_ (toMisoString ("labels " ++ letter ++ "-labels")) ] $ labelsFunc labelPoints labels
  where
    --ySpot y = if isX then y - P.axisHeight else y
    recurring = if isX then SVGA.y_ $ ms (y1 + P.fontHeight) else SVGA.x_ $ ms x1
    newPoint p = if isX then SVGA.x_ $ ms p else SVGA.y_ $ ms p
    -- TODO: zip
    labelsFunc (x:xs) (y:ys) = [SVG.text_ [ recurring , newPoint x ] [(text . toMisoString) y] ] ++ labelsFunc xs ys
    labelsFunc [] [] = []
    labelsFunc [] (_:_) = []
    labelsFunc (_:_) [] = []

pairs :: [a] -> [(a, a)]
pairs = Prelude.zip <*> Prelude.tail

makeLine :: [(Int,Int)] -> [(Int,Int)] -> Colour Double -> View Action
makeLine xp yp c = SVG.g_ [] $ pointsFunc xp yp
  where
    pointsFunc (x:xs) (y:ys) =
      let (x1, x2) = x
          (y1, y2) = y
       in [ SVG.line_ [ SVGA.stroke_ (pack . sRGB24show $ c), SVGA.x1_ $ ms x1, SVGA.x2_ $ ms x1, SVGA.x2_ $ ms x2, SVGA.y1_ $ ms y1, SVGA.y2_ $ ms y2 ] []] ++ pointsFunc xs ys
    pointsFunc [] [] = []
    pointsFunc [] (_:_) = []
    pointsFunc (_:_) [] = []

makeLegend :: [P.PlotLegend] -> MisoString -> View Action
makeLegend pl name = div_ [id_ name] $ map (\l ->
  div_ [ ] [
    div_ [ style_ $ M.fromList [ (pack "background-color", pack (sRGB24show (P.color l))), (pack "display", pack "inline-block"),
       (pack "height", pack "20px"), (pack "width", pack "20px"), (pack "border", pack "2px solid")]] []
    , span_ [] [ text . toMisoString $ P.name l ]
  ] ) pl


