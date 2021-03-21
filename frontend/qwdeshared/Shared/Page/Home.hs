{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards    #-}
module Shared.Page.Home where

import Miso
import Shared.Page.Template
import Shared.Scene.Actions
import Shared.Scene.Model
import Shared.Util.Plot

home :: Model -> View Action
-- home m@Model{..} = template header [drawPlot randomPlot tickers "random" GetRandom] m
home m@Model{..} = template header content m
  where
    content = [ (p_ [] [text "Welcome. On this site you can explore simple stock plots. Use menu in top right to navigate."]) ]
