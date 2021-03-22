{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
module Shared.Page.Plots where

import Shared.Page.Template
import Shared.Scene.Actions
import Shared.Scene.Model
import Shared.Util.Plot
import Miso

bollingerPage :: Model -> View Action
bollingerPage m@Model{..} = template header [drawPlot bollingerPlot ticks "bollingerPlot" GetBollinger] m

smaPage :: Model -> View Action
smaPage m@Model{..} = template header [drawPlot smaPlot ticks "smaplot"  GetSma] m
