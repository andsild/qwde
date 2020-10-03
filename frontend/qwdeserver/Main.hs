{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Shared.Page.Home
import Shared.Page.MissingPage
import Shared.Page.Plots
import Shared.Scene.Actions
import Shared.Scene.Model (Model(..), initialModel)
import Shared.Scene.Routes
import Util.Types.Ticker
import Control.Monad.Trans (lift, liftIO)
import           Data.Aeson (ToJSON, FromJSON)
import Data.Char (toUpper)
import Data.Maybe
import           Data.Proxy (Proxy(..))
import           Data.Text                            (Text)
import           GHC.Generics (Generic(..))
import Network.HTTP.ReverseProxy
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import           Network.HTTP.Types (status404)
import Network.Wai (Request, pathInfo)
import           Network.Wai (Application, responseLBS, rawPathInfo)
import           Network.Wai.Application.Static (defaultWebAppSettings)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Gzip (gzipFiles, gzip, def, GzipFiles (GzipCompress))
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant (Handler, (:<|>), (:<|>)(..), Tagged(..), Raw, (:>), Get, JSON, serveDirectoryWith, serve, QueryParam, ServerT)
import Servant.Client (client, Scheme(Http), ClientM, runClientM)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified System.IO                            as IO
import Html
import Manifest
import Miso (ToServerRoutes, View)
import           Lucid.Base (toHtml, renderBS)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as E

import           qualified System.Console.CmdArgs as CA


data Options = Options
  { port :: Int
  , backendPort :: Int
  } deriving (CA.Data, CA.Typeable, Show)

main :: IO ()
main = do
  Options {..} <-
    CA.cmdArgs $
    Options
    { 
      port = (8081 :: Int) CA.&= CA.help "Port to run on" :: Int
    , backendPort = (8083 :: Int) CA.&= CA.help "Port the backend is running on" :: Int
    } CA.&=
    CA.summary "test"

  IO.hPutStrLn IO.stderr $ "Running on port " ++ show port ++ " , backend as " ++ show backendPort
  manager <- newManager defaultManagerSettings
  run port $ logStdout (compress (app manager backendPort))
    where
      compress = gzip def { gzipFiles = GzipCompress }

newtype Tickers = Tickers { ticks :: [Ticker] } deriving (Show, Generic)
instance FromJSON Tickers
data SmaApi = SmaApi { prices :: [Double],  sma  :: [[Double]] } deriving (Show, Generic)
instance FromJSON SmaApi
data RandomApi = RandomApi { numbers :: [Double] } deriving (Show, Generic)
instance FromJSON RandomApi

type GatewayAPI = "tickers" :> Get '[JSON] Tickers
  :<|> "random" :> Get '[JSON] RandomApi 
  :<|> "sma" :> QueryParam "ticker" String :> QueryParam "fromDate" :>  Get '[JSON] SmaApi 
gatewayApi :: Proxy GatewayAPI
gatewayApi = Proxy

app :: Manager -> Int -> Application
app manager backendPort = serve (Proxy @ API) (static :<|> serverHandlers :<|> pure misoManifest :<|> (forwardServer manager backendPort) :<|> (Tagged handle404))
  where
    static = serveDirectoryWith (defaultWebAppSettings "static")

-- | Convert client side routes into server-side web handlers
type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action

-- | API type
type API = ("static" :> Raw)
  :<|> ServerRoutes
  :<|> ("manifest.json" :> Get '[JSON] Manifest)
  :<|> "api" :> Raw
  :<|> Raw

forwardServer :: Manager -> Int -> ServerT Raw m
forwardServer manager backendPort = Tagged $ waiProxyTo (\req -> forwardRequest req backendPort) defaultOnExc manager

forwardRequest :: Request -> Int -> IO WaiProxyResponse
forwardRequest req backendPort = do
  putStrLn $ show req
  putStrLn $ "Hello world"
  pure (WPRModifiedRequest (req { rawPathInfo = strippedApiPrefix }) (ProxyDest "127.0.0.1" backendPort))
  where
    strippedApiPrefix = Prelude.foldl (\l r -> C8.append l $ C8.cons '/' r) (C8.pack "") (map E.encodeUtf8 $ pathInfo req)

handle404 :: Application
handle404 _ respond = do
  im <- liftIO initialModel
  respond $ let v = the404 im in responseLBS status404 [("Content-Type", "text/html")] $ renderBS $ toHtml $ Wrapper $ v

serverHandlers ::
       Handler (Wrapper ((View Action)))
  :<|> Handler (Wrapper (View Action))
  :<|> Handler (Wrapper (View Action))
serverHandlers = homeHandler :<|> smaHandler :<|> bollingerHandler
     where
       send f u = do
         im <- liftIO initialModel
         pure $ Wrapper $ f im { uri = u }
       homeHandler = send home goHome
       smaHandler = send smaPage goSma
       bollingerHandler = send bollingerPage goBollinger
