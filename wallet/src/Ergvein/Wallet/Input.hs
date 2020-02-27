module Ergvein.Wallet.Input(
    Password
  , textField
  , validatedTextField
  , passField
  , passFieldWithEye
  , submitClass
  , textInputTypeDyn
  ) where

import Control.Monad (join)
import Data.Text (Text)
import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Id
import Ergvein.Wallet.Monad
import Reflex.Localize

import qualified Data.Text as T

labeledTextInput :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> TextInputConfig t
  -> m (TextInput t)
labeledTextInput lbl cfg = do
  i <- genId
  label i $ localizedText lbl
  textInput cfg {
      _textInputConfig_attributes = do
        as <- _textInputConfig_attributes cfg
        pure $ "id" =: i <> as
    }

textField :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> Text -- ^ Initial value
  -> m (Dynamic t Text)
textField lbl v0 = fmap _textInput_value $ labeledTextInput lbl def {
    _textInputConfig_initialValue = v0
  }

validatedTextField :: (MonadFrontBase t m, LocalizedPrint l0, LocalizedPrint l1)
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> m (Dynamic t Text)
validatedTextField lbl v0 mErrsD = do
  textInputValueD <- inputField
  divClass "form-field-errors" $ simpleList errsD displayError
  pure textInputValueD
  where
    errsD = fmap (maybe [] id) mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ _textInput_value <$> labeledTextInput lbl def {
      _textInputConfig_initialValue = v0
    }

displayError :: (MonadFrontBase t m, LocalizedPrint l) => Dynamic t l -> m ()
displayError errD = do
  langD <- getLanguage
  let localizedErrD = zipDynWith localizedShow langD errD
  dynText localizedErrD
  br

passField :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> m (Dynamic t Text)
passField lbl = fmap _textInput_value $ labeledTextInput lbl def {
    _textInputConfig_inputType  = "password"
  }

-- | Password field with toggleable visibility
passFieldWithEye :: (MonadFrontBase t m, LocalizedPrint l) => l -> m (Dynamic t Password)
passFieldWithEye lbl = mdo
  i <- genId
  label i $ localizedText lbl
  let initType = "password"
  typeD <- holdDyn initType $ poke eyeE $ const $ do
    v <- sampleDyn typeD
    pure $ if v == "password" then "text" else "password"
  (valD, eyeE) <- divClass "password-field" $ do
    valD <- textInputTypeDyn (updated typeD) (def &
      textInputConfig_attributes .~ pure
        (  "id"          =: i
        <> "class"       =: "eyed-field"
        <> "name"        =: ("password-" <> i)
        <> "placeholder" =: "******"
        )
      & textInputConfig_inputType .~ initType)
    passwordVisibleD <- toggle False eyeE
    let eyeButtonIconClassD = eyeButtonIconClass <$> passwordVisibleD
    eyeE <- divButton "small-eye" $ elClassDyn "i" eyeButtonIconClassD blank
    pure (valD, eyeE)
  pure valD

eyeButtonIconClass :: Bool -> Text
eyeButtonIconClass True = "far fa-eye-slash fa-fw"
eyeButtonIconClass _ = "far fa-eye fa-fw"

-- | Form submit button
submitClass :: (MonadFrontBase t m, LocalizedPrint l) => Dynamic t Text -> l -> m (Event t ())
submitClass classD lbl = do
  lblD <- localized lbl
  let classesD = do
        classVal <- classD
        lbl <- lblD
        pure $ "class" =: classVal
            <> "type"  =: "submit"
            <> "value" =: lbl
            <> "onclick" =: "return false;"
  (e, _) <- elDynAttr' "input" classesD blank
  return $ domEvent Click e

-- | Wrapper around text field that allows to switch its type dynamically with
-- saving of previous value.
textInputTypeDyn :: forall t m . MonadFrontBase t m => Event t Text -> TextInputConfig t -> m (Dynamic t Text)
textInputTypeDyn typeE cfg = fmap join $ workflow $
  go (_textInputConfig_inputType cfg) (_textInputConfig_initialValue cfg)
  where
    go :: Text -> Text -> Workflow t m (Dynamic t Text)
    go tp v0 = Workflow $ do
      tI <- textInput (cfg & textInputConfig_inputType .~ tp
                           & textInputConfig_initialValue .~ v0)
      let valD = _textInput_value tI
      let nextE = flip pushAlways typeE $ \nextTp -> do
            nextVal <- sample . current $ valD
            pure $ go nextTp nextVal
      pure (valD, nextE)
