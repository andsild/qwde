{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}
module Gateway where

import Network.HTTP.ReverseProxy
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Shared.Scene.Actions
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Util.Types.Ticker

forwardServer :: Manager -> ServerT Raw m
forwardServer manager = Tagged $ waiProxyTo forwardRequestDerp defaultOnExc manager

forwardRequestDerp :: Request -> IO WaiProxyResponse
forwardRequestDerp req = do
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  putStrLn $ show req
  pure (WPRModifiedRequest (req { rawPathInfo = strippedApiPrefix }) (ProxyDest "127.0.0.1" 8080))
  where
    strippedApiPrefix = Prelude.foldl (\l r -> C8.append l $ C8.cons '/' r) (C8.pack "") (map E.encodeUtf8 $ pathInfo req)
