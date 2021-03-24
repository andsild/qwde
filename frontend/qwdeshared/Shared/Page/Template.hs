{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
module Shared.Page.Template where

import qualified Data.Map    as M
import Shared.Scene.Actions
import Miso
import Miso.String (pack, (<>))
import Shared.Util.Constants
import Shared.Util.Web (onPreventClick)
import           Data.Bool
import           Servant.API (URI(..))
import           Shared.Scene.Model
import Shared.Scene.Routes

template :: View Action -> [View Action] -> Model -> View Action
template templateHeader content Model{..} =
  div_ [ ] ([
  hero templateHeader uri navMenuOpen
  ] ++ content ++ [
    footer
  ])

-- | Hero
hero :: View Action -> URI -> Bool -> View Action
hero content uri' navMenuOpen' =
  section_ [ class_  "hero is-warning is-bold has-text-centered" ] [
    div_ [ class_ "hero-head" ] [
     header_ [class_"nav"] [
      div_ [class_"container"] [
        div_ [class_"nav-left"][
          a_ [class_"nav-item"][]
          ],
        span_ [class_$ "nav-toggle " <> bool mempty "is-active" navMenuOpen'
              , onClick ToggleNavMenu
              ] [
          span_[][]
        , span_[][]
        , span_[][]
        ],
         div_ [ class_ $ "nav-right nav-menu " <> do  bool mempty "is-active" navMenuOpen'] headerlinks'
          ]]]
    , div_ [ class_  "hero-body" , style_ $ M.fromList [
      (pack "padding-top", pack "1rem")
      , (pack "padding-bottom", pack "2rem")
      ]] [
     div_ [ class_  "container"] [
         content
       ]
     ]
  ]
  where
    headerlinks' = map (\__@HeaderLink{..} ->
          a_ [ class_ $ "nav-item " <> do  bool mempty "is-active" (uriPath uri' == uriPath _path)
             , href_ _href, onPreventClick (ChangeURI _path) ] [ text _text ]) headerLinks

header :: View Action
header = div_ [ class_  "animated fadeIn" ] [
    a_ [ href_ githubUrl ] [
       img_ [ width_ "100"
            , class_ "animated bounceInDown"
            , src_ misoSrc
            , alt_ "miso logo"
            ]
       ]
    , h1_ [ class_  "title animated pulse"
          , style_ $ M.fromList [(pack "font-size", pack "82px")
                                ,(pack "font-weight", pack "100")
                                ,(pack "display", pack "inline")
                                ]
    ] [ text "qwde" ]
  ]

-- | Footer
footer :: View action
footer =
  footer_ [ class_  "footer", style_ $ M.fromList [
    (pack "padding-top", pack "2rem")
    , (pack "padding-bottom", pack "1rem")
    ]] [
    div_ [ class_  "container", style_ $ M.singleton "line-height" "0.2" ] [
      div_ [ class_  "content has-text-centered" ] [
         p_ [] [
         text "made using "
         , strong_ [] [ text "Miso" ]
         ,  text " by "
         ,  a_ [ href_  "https://github.com/dmjio/miso"
               , style_ $ M.singleton "color" "#363636"
               ]
              [ text "dmjio" ]
        , br_ []
        , br_ []
          ]
         , p_ [] [ a_ [href_"https://bulma.io"] [ img_
                                                    [ src_ "https://bulma.io/images/made-with-bulma.png"
                                                    , alt_ "Made with Bulma"
                                                    , width_ "128"
                                                    , height_ "22"
                                                    ]
                                                ] ]
         , p_ [] ["cat-logo by Vektora kato"]
         , p_ [] [
           a_ [ href_ githubUrl ] [ span_ [ class_"icon is-large"]
                  [
                    i_ [ class_"fa fa-github"][ ]
                  ]
                                                        ]
           ]
        ]
      ]
    ]
