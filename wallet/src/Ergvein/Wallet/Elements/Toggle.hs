module Ergvein.Wallet.Elements.Toggle(
    toggleButton
  , toggler
  ) where

import Control.Lens
import Control.Monad.Fix
import Control.Monad.IO.Class
import Reflex.Dom

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Id
import Ergvein.Wallet.Language
import Ergvein.Wallet.Util

-- | Create toggle button with pressed and unpressed states
toggleButton :: forall t m lbl . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadLocalized t m, MonadFix m, LocalizedPrint lbl)
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

-- | Toggler switch with alya materlized looking
toggler :: (DomBuilder t m, PostBuild t m, MonadSample t m, MonadIO m, MonadLocalized t m, LocalizedPrint l)
  => l
  -> (Dynamic t Bool)
  -> m  (Dynamic t Bool)
toggler lbl initialChecked = do
  let initE = updated initialChecked
  initVal <- sample $ current initialChecked
  i <- genId
  label i $ localizedText lbl
  input <- elClass "label" "switch" $ do
    input <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes %~ (\as -> "id" =: i <> "type" =: "checkbox" <> as)
      & inputElementConfig_initialChecked .~ initVal
      & inputElementConfig_setChecked .~ initE
    spanClass "slider" $ pure ()
    pure input
  pure $ _inputElement_checked input
