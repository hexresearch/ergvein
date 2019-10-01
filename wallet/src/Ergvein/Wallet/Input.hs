module Ergvein.Wallet.Input(
    textField
  , passField
  , submitClass
  ) where

import Data.Text (Text)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Env
import Ergvein.Wallet.Id
import Reflex.Dom

labeledTextInput :: MonadFront t m
  => Text -- ^ Label
  -> TextInputConfig t
  -> m (TextInput t)
labeledTextInput lbl cfg = do
  i <- genId
  label i $ text lbl
  textInput cfg {
      _textInputConfig_attributes = do
        as <- _textInputConfig_attributes cfg
        pure $ "id" =: i <> as
    }

textField :: MonadFront t m
  => Text -- ^ Label
  -> Text -- ^ Initial value
  -> m (Dynamic t Text)
textField lbl v0 = fmap _textInput_value $ labeledTextInput lbl def {
    _textInputConfig_initialValue = v0
  }

passField :: MonadFront t m
  => Text -- ^ Label
  -> m (Dynamic t Text)
passField lbl = fmap _textInput_value $ labeledTextInput lbl def {
    _textInputConfig_inputType  = "password"
  }

submitClass :: MonadFront t m
  => Text -- ^ Class
  -> Text -- ^ Label
  -> m (Event t ())
submitClass cl lbl = do
  (e, _) <- elAttr' "input" (
       "class" =: cl
    <> "type"  =: "submit"
    <> "value" =: lbl) blank
  pure $ domEvent Click e
