{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
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
import Servant.Client (client, Scheme(Http), BaseUrl(..), ClientM, runClientM, mkClientEnv)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified System.IO                            as IO
import Html
import Manifest
import Miso (ToServerRoutes, View)
import           Lucid.Base (toHtml, renderBS)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 8081..."
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (mkClientEnv manager (BaseUrl Http "localhost" 8080 ""))
  run 8081 $ logStdout (compress (app manager))
    where
      compress = gzip def { gzipFiles = GzipCompress }

queries :: ClientM Tickers
queries = getTicker
newtype Tickers = Tickers { ticks :: [Ticker] } deriving (Show, Generic)
instance FromJSON Tickers
type GatewayAPI = "tickers" :> Get '[JSON] Tickers
gatewayApi :: Proxy GatewayAPI
gatewayApi = Proxy

getTicker :: ClientM Tickers
getTicker = client gatewayApi

app :: Manager -> Application
app manager = serve (Proxy @ API) (static :<|> serverHandlers :<|> pure misoManifest :<|> (forwardServer manager) :<|> (Tagged handle404))
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

forwardServer :: Manager -> ServerT Raw m
forwardServer manager = Tagged $ waiProxyTo forwardRequest defaultOnExc manager

forwardRequest :: Request -> IO WaiProxyResponse
forwardRequest req = do
  putStrLn $ show req
  pure (WPRModifiedRequest (req { rawPathInfo = strippedApiPrefix }) (ProxyDest "127.0.0.1" 8080))
  where
    strippedApiPrefix = Prelude.foldl (\l r -> C8.append l $ C8.cons '/' r) (C8.pack "") (map E.encodeUtf8 $ pathInfo req)

handle404 :: Application -- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
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
