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
import Shared.Scene.Actions (Action(..), QwdeRandom(..), QwdeSma(..), QwdeBollinger(..), QwdeTickers(..))
import Shared.Scene.Model
import Shared.Scene.Routes
import Shared.Util.Constants (defaultColor, plotWidth, plotHeight)
import Shared.Util.Date
import Shared.Page.Home
import Shared.Page.MissingPage
import Shared.Page.Plots
import Util.Color
import Touch
import qualified Data.Time.Calendar as Time

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
        initialAction = GetTickers
        mountPoint = Nothing
        update = updateModel
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
backend = "https://qwde.no/api/"

getTickerList :: IO QwdeTickers
getTickerList = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String QwdeTickers of
    Left s -> do
      _ <- error s
      return QwdeTickers { tickers = [ "twtr" ] }
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = pack (backend ++ "tickers")
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

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

getQwdeSma :: String -> Time.Day -> Time.Day -> IO QwdeSma
getQwdeSma t fromTime toTime = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String QwdeSma of
    Left s -> error s
    Right j -> pure j
  where
    fmDate = dateToString fromTime
    toDate = dateToString toTime
    req = Request { reqMethod = GET
                  , reqURI = pack (backend ++ "sma?ticker=" ++ t ++ "&fromDate=" ++ fmDate ++ "&toDate=" ++ toDate)
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

getQwdeBollinger :: String -> Time.Day -> Time.Day -> IO QwdeBollinger
getQwdeBollinger t fromTime toTime = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String QwdeBollinger of
    Left s -> error s
    Right j -> pure j
  where
    fmDate = dateToString fromTime
    toDate = dateToString toTime
    req = Request { reqMethod = GET
                  , reqURI = pack (backend ++ "bb?ticker=" ++ t ++ "&fromDate=" ++ fmDate ++ "&toDate=" ++ toDate)
                  , reqLogin = Nothing
                  , reqHeaders = [("Content-Type", "text/plain"), ("Accept-Language", "nb-NO,nb")]
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleURI u) m = m { uri = u } <# do
  pure NoOp
updateModel (Init) m = m <# do
  pure NoOp
updateModel (ChangeURI u) m = m { navMenuOpen = False } <# do
  pushURI u
  pure NoOp
updateModel Alert m@Model{..} = m <# do
  alert $ pack (show uri)
  pure NoOp
updateModel ToggleNavMenu m@Model{..} = m { navMenuOpen = not navMenuOpen } <# do
  pure NoOp
updateModel GetTickers m@Model{..} = m <# do
  SetTickers <$> getTickerList
updateModel (SetTickers apiData) m@Model{..} = noEff m { ticks = tickers apiData }
updateModel GetRandom m@Model{..} = m <# do
  SetRandom <$> getQwdeRandom
updateModel (SetRandom apiData) m@Model{..} = noEff m { randomPlot = P.getPlot 10 plotWidth (plotHeight - 200)
    (take (length $ numbers apiData) $ map show ([1..] :: [Int]))
    ([numbers apiData])
    [P.PlotLegend "random" defaultColor]
 }
updateModel (ParseFromdate s) m@Model{..} = m <# do
  SetFromdate <$> parseStringDate (fromMisoString s)
updateModel (SetFromdate fmDate) m@Model{..} = noEff m { fromDate = fmDate }
updateModel (ParseTodate s) m@Model{..} = m <# do
  SetTodate <$> parseStringDate (fromMisoString s)
updateModel (SetTodate tDate) m@Model{..} = noEff m { toDate = tDate }
updateModel (ParseSingleTicker s) m@Model{..} = m <# do
  -- TODO: verify in model's tickerlist
  SetSingleTicker <$> return (fromMisoString s)
updateModel (SetSingleTicker s) m@Model{..} = noEff m { singleTicker = s }
updateModel GetSma m@Model{..} = m <# do
  SetSma <$> getQwdeSma singleTicker fromDate toDate
updateModel (SetSma apiData) m@Model{..} = noEff m { smaPlot = P.getPlot 10 plotWidth (plotHeight - 200)
  (take (length $ prices apiData) $ map show ([1..] :: [Int]))
  ([prices apiData] ++ (sma apiData))
  ([P.PlotLegend "sma" defaultColor ] ++ (map (\(i,c) -> P.PlotLegend (show $ i * 10) c) $ take (length $ sma apiData) (zip ([1..] :: [Int]) colorList)))
   }
updateModel GetBollinger m@Model{..} = m <# do
  SetBollinger <$> getQwdeBollinger singleTicker fromDate toDate
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
