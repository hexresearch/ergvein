module Ergvein.Wallet.Elements.Toggle(
    toggleButton
  ) where

import Reflex.Dom
import Control.Monad.Fix

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Util

-- | Create toggle button with pressed and unpressed states
toggleButton :: forall t m lbl . (DomBuilder t m, MonadHold t m, MonadLocalized t m, MonadFix m, LocalizedPrint lbl)
  => lbl -- ^ Label of button unpressed
  -> lbl -- ^ Label of button pressed
  -> Dynamic t Bool -- ^ Input of toggler
  -> m (Dynamic t Bool)
toggleButton lblOff lblOn val0D = mdo
  valD <- mergeDyn val0D $ poke clickE $ const $ do
    val <- sample . current $ valD
    pure $ not val
  valD' <- holdUniqDyn valD
  clickE <- fmap switchDyn $ widgetHoldDyn $ ffor valD' $ \v -> if v
    then buttonClass "button button-outline button-on" lblOn
    else buttonClass "button button-outline button-off" lblOff
  pure valD'
