{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Common as C
import qualified Data.Graph.Plotter as P

import Touch

import Data.Colour (Colour)
import Data.Colour.Names
import Control.Arrow
import Data.Proxy
import qualified Data.Map as M
import Miso hiding (defaultOptions)
import Miso.String hiding (map, length, take, zip)
import JavaScript.Web.XMLHttpRequest (Request(..), RequestData(..), xhrByteString, Method(..), contents)
import Data.Aeson

import qualified Widget.Flatpickr as Flatpickr

main :: IO ()
main = miso $ \currentURI -> App
    { model = C.Model currentURI False (0,0)
        (P.getPlot 10 C.plotWidth C.plotHeight (map show ([1..10] :: [Int])) ([[1..10]] :: [[Double]]) ([P.PlotLegend "" C.defaultColor]))
        (P.getPlot 10 C.plotWidth C.plotHeight (map show ([1..10] :: [Int])) ([[1..10]] :: [[Double]]) ([P.PlotLegend "" C.defaultColor]))
        (P.getPlot 10 C.plotWidth C.plotHeight (map show ([1..10] :: [Int])) ([[1..10]] :: [[Double]]) ([P.PlotLegend "" C.defaultColor]))
    , view = viewModel
    , ..
    }
      where
        initialAction = C.NoOp
        mountPoint = Nothing
        update = updateModel
        --events = defaultEvents
        --subs = [ uriSub C.HandleURI ]
        events = M.insert (pack "mousemove") False $
                 M.insert (pack "touchstart") False $
                 M.insert (pack "touchmove") False defaultEvents
        subs = [ uriSub C.HandleURI, (mouseSub C.HandleMouse) ]
        viewModel m =
          case runRoute (Proxy :: Proxy C.ClientRoutes) C.handlers C.uri m of
            Left _ -> C.the404 m
            Right v -> v

instance FromJSON C.QwdeRandom where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON C.QwdeSma where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON C.QwdeBollinger where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

backend =
#if PRODUCTION
  "http://qwde.no:8080/"
#else
  "http://localhost.no:8080/"
#endif

getQwdeRandom :: IO C.QwdeRandom
getQwdeRandom = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String C.QwdeRandom of
    Left s -> error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack (backend ++ "/random")
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

getQwdeSma :: IO C.QwdeSma
getQwdeSma = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String C.QwdeSma of
    Left s -> error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack (backend ++ "sma/twtr/20150102?toDate=20170301")
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

getQwdeBollinger :: IO C.QwdeBollinger
getQwdeBollinger = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String C.QwdeBollinger of
    Left s -> error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack (backend ++ "bb/twtr/20150102?toDate=20170301")
                  , reqLogin = Nothing
                  , reqHeaders = [("Content-Type", "text/plain"), ("Accept-Language", "nb-NO,nb")]
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

updateModel :: C.Action -> C.Model -> Effect C.Action C.Model
updateModel (C.HandleURI u) m = m { C.uri = u } <# do
  pure C.NoOp
updateModel (C.ChangeURI u) m = m { C.navMenuOpen = False } <# do
  pushURI u
  pure C.NoOp
updateModel C.Alert m@C.Model{..} = m <# do
  alert $ pack (show uri)
  pure C.NoOp
updateModel C.ToggleNavMenu m@C.Model{..} = m { C.navMenuOpen = not navMenuOpen } <# do
  pure C.NoOp
updateModel C.GetRandom m@C.Model{..} = m <# do
  C.SetRandom <$> getQwdeRandom
updateModel (C.SetRandom apiData) m@C.Model{..} = noEff m { C.randomPlot = P.getPlot 10 C.plotWidth (C.plotHeight - 200)
    (take (length $ C.numbers apiData) $ map show ([1..] :: [Int]))
    ([C.numbers apiData])
    [P.PlotLegend "random" C.defaultColor]
 }
updateModel C.GetSma m@C.Model{..} = m <# do
  C.SetSma <$> getQwdeSma
updateModel (C.SetSma apiData) m@C.Model{..} = noEff m { C.smaPlot = P.getPlot 10 C.plotWidth (C.plotHeight - 200)
  (take (length $ C.prices apiData) $ map show ([1..] :: [Int]))
  ([C.prices apiData] ++ (C.sma apiData))
  ([P.PlotLegend "sma" C.defaultColor ] ++ (map (\(i,c) -> P.PlotLegend (show $ i * 10) c) $ take (length $ C.sma apiData) (zip ([1..] :: [Int]) colorList)))
   }
updateModel C.GetBollinger m@C.Model{..} = m <# do
  C.SetBollinger <$> getQwdeBollinger
updateModel (C.SetBollinger apiData) m@C.Model{..} = noEff m { C.bollingerPlot = P.getPlot 10 C.plotWidth (C.plotHeight - 200)
  (take (length $ C.price apiData) $ map show ([1..] :: [Int]))
  [C.price apiData, C.lowerBand apiData, C.upperBand apiData, C.mean apiData]
  [P.PlotLegend "price" C.defaultColor, P.PlotLegend "lower" (colorList !! 10), P.PlotLegend "upper" (colorList !! 11), P.PlotLegend "mean" (colorList !! 13)]
   }
updateModel C.NoOp m = noEff m
updateModel (C.HandleTouch (TouchEvent touch)) model =
  model <# do
    putStrLn "Touch did move"
    print touch
    return $ C.HandleMouse $ trunc . page $ touch
updateModel (C.HandleMouse newCoords) model =
  noEff model { C.mouseCords = newCoords }

trunc :: (Double, Double) -> (Int, Int)
trunc = truncate *** truncate


colorList :: [Colour Double]
colorList = [
  aliceblue
  , antiquewhite
  , aqua
  , aquamarine
  , azure
  , beige
  , bisque
  , blanchedalmond
  , blue
  , blueviolet
  , brown
  , burlywood
  , cadetblue
  , chartreuse
  , chocolate
  , coral
  , cornflowerblue
  , cornsilk
  , crimson
  , cyan
  , darkblue
  , darkcyan
  , darkgoldenrod
  , darkgray
  , darkgreen
  , darkgrey
  , darkkhaki
  , darkmagenta
  , darkolivegreen
  , darkorange
  , darkorchid
  , darkred
  , darksalmon
  , darkseagreen
  , darkslateblue
  , darkslategray
  , darkslategrey
  , darkturquoise
  , darkviolet
  , deeppink
  , deepskyblue
  , dimgray
  , dimgrey
  , dodgerblue
  , firebrick
  , floralwhite
  , forestgreen
  , fuchsia
  , gainsboro
  , ghostwhite
  , gold
  , goldenrod
  , gray
  , grey
  , green
  , greenyellow
  , honeydew
  , hotpink
  , indianred
  , indigo
  , ivory
  , khaki
  , lavender
  , lavenderblush
  , lawngreen
  , lemonchiffon
  , lightblue
  , lightcoral
  , lightcyan
  , lightgoldenrodyellow
  , lightgray
  , lightgreen
  , lightgrey
  , lightpink
  , lightsalmon
  , lightseagreen
  , lightskyblue
  , lightslategray
  , lightslategrey
  , lightsteelblue
  , lightyellow
  , lime
  , limegreen
  , linen
  , magenta
  , maroon
  , mediumaquamarine
  , mediumblue
  , mediumorchid
  , mediumpurple
  , mediumseagreen
  , mediumslateblue
  , mediumspringgreen
  , mediumturquoise
  , mediumvioletred
  , midnightblue
  , mintcream
  , mistyrose
  , moccasin
  , navajowhite
  , navy
  , oldlace
  , olive
  , olivedrab
  , orange
  , orangered
  , orchid
  , palegoldenrod
  , palegreen
  , paleturquoise
  , palevioletred
  , papayawhip
  , peachpuff
  , peru
  , pink
  , plum
  , powderblue
  , purple
  , red
  , rosybrown
  , royalblue
  , saddlebrown
  , salmon
  , sandybrown
  , seagreen
  , seashell
  , sienna
  , silver
  , skyblue
  , slateblue
  , slategray
  , slategrey
  , snow
  , springgreen
  , steelblue
  , Data.Colour.Names.tan
  , teal
  , thistle
  , tomato
  , turquoise
  , violet
  , wheat
  , white
  , whitesmoke
  , yellow
  , yellowgreen
  ]

