module Shared.Util.Web where

import Miso
import Miso.String (toMisoString)
import Shared.Scene.Actions (Action(..))

onPreventClick :: Action -> Attribute Action
onPreventClick action =
  onWithOptions defaultOptions { preventDefault = True }
    (toMisoString "click") emptyDecoder (\() -> action)
