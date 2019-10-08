module Ergvein.Wallet.Input(
    textField
  , passField
  , submitClass
  ) where

import Data.Text (Text)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Id
import Ergvein.Wallet.Monad
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

-- | Form submit button
submitClass :: MonadFront t m => Dynamic t Text -> Dynamic t Text -> m (Event t ())
submitClass classD lblD = do
  let classesD = do
        classVal <- classD
        lbl <- lblD
        pure $ "class" =: classVal
            <> "type"  =: "submit"
            <> "value" =: lbl
  (e, _) <- elDynAttr' "input" classesD blank
  return $ domEvent Click e
