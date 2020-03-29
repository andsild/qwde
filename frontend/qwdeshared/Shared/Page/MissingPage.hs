{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
module Shared.Page.MissingPage where

import Shared.Page.Template
import Miso
import Miso.String
import Shared.Util.Constants
import Shared.Scene.Actions
import Shared.Scene.Model
import qualified Data.Map    as M
import Shared.Scene.Routes (goHome)
import Shared.Util.Web (onPreventClick)

the404 :: Model -> View Action
the404 m = template header404 [content] m
  where
    header404 = div_ [] [ a_ [ href_ githubUrl ] [ img_ [
           width_  "100"
         , class_  "animated bounceOutUp"
         , src_ misoSrc
         , alt_ "miso logo"
         ]]
         , h1_ [ class_  "title"
               , style_ $ M.fromList [(pack "font-size", pack "82px")
                                     ,(pack "font-weight", pack "100")
                                     ]
         ] [ text "404" ]
       , h2_ [ class_  "subtitle animated pulse" ] [
          text "No soup for you! "
          , a_ [ href_ "/", onPreventClick (ChangeURI goHome) ] [ text " - Go Home" ]
         ]
       ]
    content = p_ [] [text ":(" ]
    --result = div_ [] [header404, content]
