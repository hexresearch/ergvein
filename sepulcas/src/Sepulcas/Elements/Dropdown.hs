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

dropdownContainer :: forall t m lbl a . ( MonadIO m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadLocalized t m, MonadFix m, LocalizedPrint lbl)
  => lbl
  -> lbl 
  -> Dynamic t Bool 
  -> m a
  -> m (Dynamic t (Maybe a))
dropdownContainer lClosed lOpened tglD innerContent = mdo
  valD <- mergeDyn tglD $ poke clickE $ const $ do
    val <- sample . current $ valD
    pure $ not val
  valD' <- holdUniqDyn valD
  clickE <- fmap switchDyn $ networkHoldDyn $ ffor valD' $ \v -> if v
    then divButton "dropdown-header" $ do
        localizedText lClosed
        text " "
        spanClass "fa fa-fw fa-chevron-up" $ blank
    else divButton "dropdown-header" $ do
        localizedText lOpened
        text " "
        spanClass "fa fa-fw fa-chevron-down" $ blank
  networkHoldDyn $ ffor valD' $ \v -> if v
    then do
      Just <$> innerContent
    else do
      pure Nothing