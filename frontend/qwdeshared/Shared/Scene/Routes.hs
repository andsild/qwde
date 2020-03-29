{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators        #-}
module Shared.Scene.Routes where

import Shared.Scene.Actions
import Shared.Scene.Model
import           Data.Proxy
import Miso (View(..))
import           Servant.API (URI(..), (:<|>), (:>), safeLink, (:<|>)(..))
#if MIN_VERSION_servant(0,17,0)
import          Servant.Links (linkURI)
#else
import	  Servant.Utils.Links (linkURI)
#endif
import Miso.String

-- | Client Routes
type Home = View Action
type Sma  = "sma" :> View Action
type Bollinger  = "bollinger" :> View Action

type ClientRoutes = Home :<|> Sma :<|> Bollinger

-- | Links
goHome, goSma, goBollinger :: URI
(goHome, goSma, goBollinger) = (
  linkURI (safeLink routes homeProxy)
  , linkURI (safeLink routes smaProxy)
  , linkURI (safeLink routes bollingerProxy)
  )

homeProxy :: Proxy Home
homeProxy = Proxy
smaProxy :: Proxy Sma
smaProxy = Proxy
bollingerProxy :: Proxy Bollinger
bollingerProxy = Proxy
routes :: Proxy ClientRoutes
routes = Proxy

data HeaderLink = HeaderLink {
    _href :: MisoString
    , _text :: MisoString
    , _path :: URI
}

headerLinks :: [HeaderLink]
headerLinks = [
    HeaderLink "/" "Home" goHome
    , HeaderLink "/sma" "SMA" goSma
    , HeaderLink "/bollinger" "Bollinger" goBollinger
    ]

