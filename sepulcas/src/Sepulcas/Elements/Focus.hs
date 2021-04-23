module Sepulcas.Elements.Focus(
    selElementFocus
  ) where

import Data.Functor
import Data.Text (Text)
import Language.Javascript.JSaddle hiding ((!!))
import Reflex.Dom

selElementFocus :: MonadJSM m => RawElement GhcjsDomSpace -> m ()
selElementFocus rawEl = liftJSM $ do
  void $ eval func
  void $ jsg1 funcName (toJSVal rawEl)
  where
    funcName = "ergvein_set_el_focus" :: Text
    func = " ergvein_set_el_focus = function(el) {el.focus();}" :: Text
