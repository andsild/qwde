{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Aeson (eitherDecodeStrict)
import Control.Arrow
import qualified Data.Graph.Plotter as P
import qualified Data.Map as M
import Data.Proxy
import JavaScript.Web.XMLHttpRequest (Request(..), RequestData(..), xhrByteString, Method(..), contents)
import Miso hiding (defaultOptions)
import Miso.String hiding (map, length, take, zip)
import Servant.API ((:<|>), (:<|>)(..))
import Shared.Scene.Actions (Action(..), QwdeRandom(..), QwdeSma(..), QwdeBollinger(..))
import Shared.Scene.Model
import Shared.Scene.Routes
import Shared.Util.Constants (defaultColor, plotWidth, plotHeight)
import Shared.Page.Home
import Shared.Page.MissingPage
import Shared.Page.Plots
import Util.Color
import Touch
import qualified Widget.Flatpickr as Flatpickr
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time

handlers :: (Model -> View Action) :<|> ((Model -> View Action) :<|> (Model -> View Action))
handlers = home :<|> smaPage :<|> bollingerPage

main :: IO ()
main = do
  initModel <- initialModel
  miso $ \_ -> App
    { model = initModel
    , view = viewModel
    , ..
    }
      where
        initialAction = NoOp
        mountPoint = Nothing
        update = updateModel
        --events = defaultEvents
        --subs = [ uriSub C.HandleURI ]
        logLevel = DebugPrerender
        events = M.insert (pack "mousemove") False $
                 M.insert (pack "touchstart") False $
                 M.insert (pack "touchmove") False defaultEvents
        subs = [ uriSub HandleURI, (mouseSub HandleMouse) ]
        viewModel m =
          case runRoute (Proxy :: Proxy ClientRoutes) handlers uri m of
            Left _ -> the404 m
            Right v -> v

backend :: String
backend = "http://localhost:8083/api/"

getQwdeRandom :: IO QwdeRandom
getQwdeRandom = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String QwdeRandom of
    Left s -> error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack (backend ++ "random")
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

getQwdeSma :: Time.Day -> IO QwdeSma
getQwdeSma time = do
  Just resp <- contents <$> xhrByteString (req time)
  case eitherDecodeStrict resp :: Either String QwdeSma of
    Left s -> error s
    Right j -> pure j
  where
    req t = Request { reqMethod = GET
                  , reqURI = pack (backend ++ "sma?ticker=" ++ "twtr" ++ "&fromDate=20150102&toDate=20170301")
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

getQwdeBollinger :: IO QwdeBollinger
getQwdeBollinger = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String QwdeBollinger of
    Left s -> error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack (backend ++ "bb?ticker=twtr&fromDate=20150102&toDate=20170301")
                  , reqLogin = Nothing
                  , reqHeaders = [("Content-Type", "text/plain"), ("Accept-Language", "nb-NO,nb")]
                  , reqWithCredentials = False
                  , reqData = NoData
                  }
parseStringDate :: String -> IO Time.Day
parseStringDate s = undefined

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleURI u) m = m { uri = u } <# do
  pure NoOp
updateModel (ChangeURI u) m = m { navMenuOpen = False } <# do
  pushURI u
  pure NoOp
updateModel Alert m@Model{..} = m <# do
  alert $ pack (show uri)
  pure NoOp
updateModel ToggleNavMenu m@Model{..} = m { navMenuOpen = not navMenuOpen } <# do
  pure NoOp

--updateModel (Init) m@Model{..} = m <# do
  --Flatpickr.viewModel (flatpickrIface $ m ^. mDate) $ m ^. mFlatpickr

updateModel GetRandom m@Model{..} = m <# do
  SetRandom <$> getQwdeRandom
updateModel (SetRandom apiData) m@Model{..} = noEff m { randomPlot = P.getPlot 10 plotWidth (plotHeight - 200)
    (take (length $ numbers apiData) $ map show ([1..] :: [Int]))
    ([numbers apiData])
    [P.PlotLegend "random" defaultColor]
 }
updateModel (ParseFromdate s) m@Model{..} = m <# do
  SetFromdate <$> parseStringDate (show s)
updateModel GetSma m@Model{..} = m <# do
  SetSma <$> getQwdeSma fromDate
updateModel (SetSma apiData) m@Model{..} = noEff m { smaPlot = P.getPlot 10 plotWidth (plotHeight - 200)
  (take (length $ prices apiData) $ map show ([1..] :: [Int]))
  ([prices apiData] ++ (sma apiData))
  ([P.PlotLegend "sma" defaultColor ] ++ (map (\(i,c) -> P.PlotLegend (show $ i * 10) c) $ take (length $ sma apiData) (zip ([1..] :: [Int]) colorList)))
   }
updateModel GetBollinger m@Model{..} = m <# do
  SetBollinger <$> getQwdeBollinger
updateModel (SetBollinger apiData) m@Model{..} = noEff m { bollingerPlot = P.getPlot 10 plotWidth (plotHeight - 200)
  (take (length $ price apiData) $ map show ([1..] :: [Int]))
  [price apiData, lowerBand apiData, highBand apiData, mean apiData]
  [P.PlotLegend "price" defaultColor, P.PlotLegend "lower" (colorList !! 10), P.PlotLegend "upper" (colorList !! 11), P.PlotLegend "mean" (colorList !! 13)]
   }
updateModel NoOp m = noEff m
updateModel (HandleTouch (TouchEvent touch)) model =
  model <# do
    putStrLn "Touch did move"
    print touch
    return $ HandleMouse $ trunc . page $ touch
updateModel (HandleMouse newCoords) model =
  noEff model { mouseCords = newCoords }

trunc :: (Double, Double) -> (Int, Int)
trunc = truncate *** truncate
