module Sepulcas.Elements.Dropdown (
   dropdownContainer
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Reflex.Dom
import Reflex.Flunky
import Reflex.Localize.Dom
import Sepulcas.Elements.Button
import Sepulcas.Elements.Markup
import Data.Text

visibilityClass :: Text -> Bool -> Text
visibilityClass classes True = classes <> " dropdownContainerHidden"
visibilityClass classes False = classes

dropdownContainer :: forall t m lbl a . ( MonadIO m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadLocalized t m, MonadFix m, LocalizedPrint lbl)
  => lbl
  -> lbl
  -> Dynamic t Bool
  -> m a
  -> m a
dropdownContainer lClosed lOpened tglD innerContent = mdo
  valD <- mergeDyn tglD $ poke clickE $ const $ not <$> sampleDyn valD
  valD' <- holdUniqDyn valD
  clickE <- fmap switchDyn $ networkHoldDyn $ ffor valD' $ \v ->
    divClass "dropdown-header-containter" $ divButton "dropdown-header" $ elClass "h5" "mb-0" $ do
      let (lbl, icon) = if v then (lClosed, "arrow_drop_down") else (lOpened, "arrow_drop_up")
      localizedText lbl
      text " "
      materialIconRound icon
  let backButtonClassesD = visibilityClass "" <$> valD'
  elClassDyn "div" backButtonClassesD innerContent