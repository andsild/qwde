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

drawPlot :: P.Plot -> [String] -> [Char] -> Action -> Bool -> View Action
drawPlot plot tickers name action isDataFetching' =
  div_ [ class_  "content has-text-centered" ] ([
                -- Not supposed to do the line below in Haskell/miso but hey ho
                script_ [] (toMisoString ("document.dispatchEvent(new CustomEvent('QwdePlotLoad'))" :: String))
                --div_ [ id_ . toMisoString $ (name ++ "cal") ] []
                , "From date: "
                , input_ [ type_ "date", id_ "fromDatePicker", onChange $ ParseFromdate ]
                , " To date: "
                , input_ [ type_ "date", id_ "toDatePicker", onChange $ ParseTodate ]
                , " Instrument: "
                , select_ [ id_ "tickerPicker", onChange $ ParseSingleTicker ] 
                    [ 
                      optgroup_ [ prop "label" (toMisoString ("Stock Tickers" :: String)) ] 
                      (map (\x -> option_ [ value_ (toMisoString x) ] [ text (toMisoString x) ] ) tickers)
                    ]
                , br_ []
                , button_ [ id_ (toMisoString $ name ++ "btn"), onClick action, disabled_ isDataFetching' ] [ text "Render plot" ]
                , div_ [ id_ . toMisoString $ (name ++ "id") ] [
                    SVG.svg_ [ class_ "graph", SVGA.visibility_ showGraph] plotArea
                ]
                , div_ [id_ (toMisoString (name ++ "Legend" :: String))] legend
                ])
  where
    showGraph = if (Prelude.null $ P.plotData plot) then "hidden" else "visible"
    legend = if (Prelude.null $ P.plotData plot) then [] else makeLegend (P.legend plot)
    plotArea = if (Prelude.null $ P.plotData plot)
               then []
               else ([
                    makeAxis True (P.xAxis plot)
                         , makeAxis False (P.yAxis plot)
                         , makeLabelpoints True (P.xAxis plot)
                         , makeLabelpoints False (P.yAxis plot)
                         ] ++ (map (\(p,l) -> makeLine (pairs $ P.xTicks p) (pairs $ P.yTicks p) (P.color l)) (zip (P.plotData plot) (P.legend plot))))

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

makeLegend :: [P.PlotLegend] -> [View Action]
makeLegend pl = map (\l ->
  div_ [ ] [
    div_ [ style_ $ M.fromList [ (pack "background-color", pack (sRGB24show (P.color l))), (pack "display", pack "inline-block"),
       (pack "height", pack "20px"), (pack "width", pack "20px"), (pack "border", pack "2px solid")]] []
    , span_ [] [ text . toMisoString $ P.name l ]
  ] ) pl
