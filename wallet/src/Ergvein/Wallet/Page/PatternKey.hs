module Ergvein.Wallet.Page.PatternKey(
    patternKeyWidget
  ) where

import Ergvein.Crypto.Keys     (Mnemonic)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Password
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Storage.Data
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Storage.AuthInfo
import Reflex.Localize

import Reflex.Dom.FragmentShaderCanvas
import Reflex.Dom
import qualified Data.Text as T

patternKeyWidget :: MonadFrontBase t m => m ()
patternKeyWidget = wrapper False $ mdo
  buildE <- delay 0.1 =<< getPostBuild
  myTextD <- holdDyn "" $ mytext <$ buildE
  (dError :: Dynamic t (Maybe Text)) <- divClass "right" $ fragmentShaderCanvas
        -- Here we determine the resolution of the canvas
        -- It would be desireable to do so dynamically, based on the widget
        -- size. But Reflex.Dom.Widget.Resize messes with the CSS layout.
        (mconcat [ "width"  =: "1000" , "height" =: "1000" ])
        myTextD
  divClass "another" $ text "lolmytest"
  pure ()

mytext :: T.Text
mytext = "preprecision mediump float;uniform vec2 u_windowSize;void main() {float s = 2.0 / min(u_windowSize.x, u_windowSize.y);vec2 pos = s * (gl_FragCoord.xy - 0.5 * u_windowSize);if (length(pos) > 1.0) { gl_FragColor = vec4(0,0,0,0); return; }gl_FragColor = vec4(1.0,0.0,0.0,1.0);}"
