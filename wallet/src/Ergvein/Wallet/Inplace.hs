module Ergvein.Wallet.Inplace(
    EditAction(..)
  , InplaceEditCfg(..)
  , inplaceEditField
  ) where

import Control.Monad.Fix
import Data.Functor (void)
import Data.Text (Text)
import Reflex.Dom

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language

-- | Output action for 'inplaceEditField'
data EditAction a =
    EditDelete !a  -- ^ Delete given value from upstream. You should switch out the widget manually.
  | EditUpdate !a !a -- ^ Update value upstream. First is old value, the second is new one.
  deriving (Eq, Show)

data InplaceEditCfg t lbl = InplaceEditCfg {
  _inplaceCanDelete   :: Dynamic t Bool -- ^ Delete button is visible
, _inplaceShowClass   :: Dynamic t Text -- ^ Class of show label
, _inplaceSeparator   :: Dynamic t (Maybe Text) -- ^ Draw a separator after buttons with given classes
, _inplaceEditLabel   :: lbl  -- ^ Label text for eidt button
, _inplaceEditClass   :: Dynamic t Text -- ^ Edit button classes
, _inplaceBtnGroup    :: Dynamic t Text -- ^ Class of button group in edit stage
, _inplaceErrorClass  :: Dynamic t Text -- ^ Class for validation errors
, _inplaceSaveLabel   :: lbl -- ^ Save button label
, _inplaceDeleteLabel :: lbl -- ^ Delete button label
, _inplaceCancelLabel :: lbl -- ^ Cancel button label
}

-- | Generic widget to edit a single field value inplace. Text label with a edit button aside.
-- On edit label is replaced with text field and save/delete/cancel buttons below.
inplaceEditField :: forall t m a lbl . (DomBuilder t m, MonadFix m, MonadHold t m, MonadLocalized t m, LocalizedPrint lbl)
  => InplaceEditCfg t lbl -- ^ Configuration of widget
  -> (a -> Text) -- ^ Render value to text
  -> (Text -> Either lbl a) -- ^ Parse value from text
  -> Dynamic t a -- ^ Initial value with feature of updating from outside
  -> m (Event t (EditAction a))
inplaceEditField InplaceEditCfg{..} renderVal parseVal val0D = mdo
  tglD <- holdDyn False editE
  valD <- mergeDyn val0D $ fforMaybe actE $ \case
    EditUpdate _ val -> Just val
    _ -> Nothing
  actD <- widgetHoldDyn $ ffor tglD $ \case
    True -> editWidget valD
    False -> showWidget valD
  let (editE, actE) = (\(a,b) -> (switchDyn a, switchDyn b)) $ splitDynPure actD
  pure actE
  where
    showWidget :: Dynamic t a -> m (Event t Bool, Event t (EditAction a))
    showWidget valD = do
      divClassDyn _inplaceShowClass $ dynText $ renderVal <$> valD
      editE <- buttonClass _inplaceEditClass _inplaceEditLabel
      pure (True <$ editE, never)

    editWidget :: Dynamic t a -> m (Event t Bool, Event t (EditAction a))
    editWidget valD = el "div" $ do
      val0 <- sample . current $ valD
      textD <- fmap _inputElement_value $ inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "text")
        & inputElementConfig_initialValue .~ renderVal val0
        & inputElementConfig_setValue .~ (renderVal <$> updated valD)
      (goE, delE, closeE) <- divClassDyn _inplaceBtnGroup $ do
        goE <- outlineButton _inplaceSaveLabel
        delE <- fmap switchDyn $ widgetHoldDyn $ ffor _inplaceCanDelete $ \b ->
          if b then outlineButton _inplaceDeleteLabel else pure never
        closeE <- outlineButton _inplaceCancelLabel
        pure (goE, delE, closeE)
      setE <- validateVal $ current textD `tag` goE
      _ <- widgetHoldDyn $ ffor _inplaceSeparator $ \msep -> case msep of
        Nothing -> pure ()
        Just sep -> elClass "hr" sep $ pure ()
      let setWithOldE = attachWith EditUpdate (current valD) setE
          delWithOldE = EditDelete <$> tag (current valD) delE
      let actE = leftmost [delWithOldE, setWithOldE]
      pure (False <$ closeE, actE)

    validateVal :: Event t Text -> m (Event t a)
    validateVal txtE = do
      let valE = parseVal <$> txtE
      void $ widgetHold (pure ()) $ ffor valE $ \case
        Left er -> divClassDyn _inplaceErrorClass $ localizedText er
        _ -> pure ()
      pure $ fmapMaybe (either (const Nothing) Just) valE
