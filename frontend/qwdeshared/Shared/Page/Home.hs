{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards    #-}
module Shared.Page.Home where

import Miso
import Shared.Page.Template
import Shared.Scene.Actions
import Shared.Scene.Model

home :: Model -> View Action
-- home m@Model{..} = template header [drawPlot randomPlot tickers "random" GetRandom] m
home m@Model{..} = template header content m
  where
    content = [ 
      (div_ [ class_  "content has-text-centered" ] [
        (p_ [] [text "Welcome"])
          , (p_ [] [text "On this site you can explore simple stock plots." ])
          , (p_ [] [text "Use menu in top right to navigate." ])
        ])
      ]
