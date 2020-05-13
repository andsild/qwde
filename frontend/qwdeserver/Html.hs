{-# LANGUAGE OverloadedStrings          #-}
module Html where

import qualified Lucid                                as L
import           Lucid.Base (toHtml, makeAttribute)
import           Miso()
import           Miso.String hiding (map)
import qualified            Data.Graph.Plotter as P
import Shared.Util.Constants (plotHeight, plotWidth)

-- | Wrapper for setting HTML doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (Wrapper a) where
  toHtmlRaw = L.toHtml
  toHtml (Wrapper x) = do
      L.doctype_
      L.html_ [ L.lang_ "en" ] $ do
        L.head_ $ do
          L.title_ "qwde"
          L.link_ [ L.rel_ "stylesheet"
                  , L.href_ "static/gh-fork-ribbon.min.css"
                  ]
          L.link_ [ L.rel_ "manifest"
                  , L.href_ "/manifest.json"
                  ]
          L.meta_ [ L.httpEquiv_ "content-type", L.content_ "text/html; charset=utf-8" ]
          L.meta_ [ L.charset_ "utf-8" ]
          L.meta_ [ L.name_ "theme-color", L.content_ "#00d1b2" ]
          L.meta_ [ L.httpEquiv_ "X-UA-Compatible"
                  , L.content_ "IE=edge"
                  ]
          L.meta_ [ L.name_ "viewport"
                  , L.content_ "width=device-width, initial-scale=1"
                  ]
          L.meta_ [ L.name_ "description"
                  , L.content_ "qwde is a work in progress"
                  ]
          L.style_ ((pack ("body{font-family:'Open Sans', sans-serif;}.graph .labels.x-labels{text-anchor:middle;}.graph .labels.y-labels{text-anchor:end;}.graph{height:")
            <> (pack $ show plotHeight)
            <> (pack "px;width:")
            <> (pack $ show plotWidth) <> (pack "px;}.graph .grid{stroke:#ccc;stroke-dasharray:0;stroke-width:1;}.labels{font-size:")
            <> (pack $ show P.fontHeight) <> (pack "px;}.label-title{font-weight:bold;text-transform:uppercase;font-size:12px;fill:black;}.data{fill:red;stroke-width:1;}")))
          cssRef animateRef
          cssRef bulmaRef
          cssRef fontAwesomeRef
          cssRef flatpickrRef
          jsRef "/static/buttons.js"
          jsRef "/static/all.js"
          jsRef "https://cdn.jsdelivr.net/npm/flatpickr"
        L.body_ (L.toHtml x)
          where
            jsRef href =
              L.with (L.script_ mempty)
                [ makeAttribute "src" href
                , makeAttribute "async" mempty
                , makeAttribute "defer" mempty
                ]
            cssRef href =
              L.with (L.link_ mempty) [
                  L.rel_ "stylesheet"
                , L.type_ "text/css"
                , L.href_ href
                ]

fontAwesomeRef :: MisoString
fontAwesomeRef = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"

flatpickrRef :: MisoString
flatpickrRef = "https://cdn.jsdelivr.net/npm/flatpickr/dist/flatpickr.min.css"

animateRef :: MisoString
animateRef = "static/animate.min.css"

bulmaRef :: MisoString
bulmaRef = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"