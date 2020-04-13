module Ergvein.Wallet.Input(
    Password
  , textField
  , validatedTextField
  , validatedTextFieldSetVal
  , textFieldSetValValidated
  , textFieldValidated
  , passField
  , passFieldWithEye
  , submitClass
  , textInputTypeDyn
  ) where

import Control.Monad (join)
import Control.Lens
import Data.Text (Text)
import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Id
import Ergvein.Wallet.Monad
import Reflex.Localize

import qualified Data.Text as T

labeledTextInput :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> InputElementConfig EventResult t (DomBuilderSpace m)
  -> m (InputElement EventResult (DomBuilderSpace m) t)
labeledTextInput lbl cfg = do
  i <- genId
  label i $ localizedText lbl
  inputElement $ cfg
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
      %~ (\as -> "id" =: i <> "type" =: "text" <> as)

textField :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> Text -- ^ Initial value
  -> m (Dynamic t Text)
textField lbl v0 = fmap _inputElement_value $ labeledTextInput lbl $ def
  & inputElementConfig_initialValue .~ v0

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
    inputField = divClassDyn isInvalidD $ textField lbl v0

validatedTextFieldSetVal :: (MonadFrontBase t m, LocalizedPrint l0, LocalizedPrint l1)
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> Event t Text
  -> m (Dynamic t Text)
validatedTextFieldSetVal lbl v0 mErrsD setValE = do
  textInputValueD <- inputField
  divClass "form-field-errors" $ simpleList errsD displayError
  pure textInputValueD
  where
    errsD = fmap (maybe [] id) mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ fmap _inputElement_value $ labeledTextInput lbl $ def
      & inputElementConfig_initialValue .~ v0
      & inputElementConfig_setValue .~ setValE

textFieldSetValValidated :: (MonadFrontBase t m, LocalizedPrint l0, LocalizedPrint l1, Show a)
  => l0 -- ^ Label
  -> a -- ^ Initial value
  -> Event t a -- ^ Set value event
  -> (Text -> Either [l1] a) -- ^ Validatior
  -> m (Dynamic t a) -- ^ Only valid values get through
textFieldSetValValidated lbl v0 setValE f = mdo
  mErrsD <- holdDyn Nothing $ fmap (either Just (const Nothing)) rawE
  txtD <- validatedTextFieldSetVal lbl (showt v0) mErrsD (showt <$> setValE)
  let rawE = updated $ f <$> txtD
  holdDyn v0 $ fmapMaybe (either (const Nothing) Just) rawE

textFieldValidated :: (MonadFrontBase t m, LocalizedPrint l0, LocalizedPrint l1, Show a)
  => l0 -- ^ Label
  -> a -- ^ Initial value
  -> (Text -> Either [l1] a) -- ^ Validatior
  -> m (Dynamic t a) -- ^ Only valid values get through
textFieldValidated lbl v0 f = mdo
  mErrsD <- holdDyn Nothing $ fmap (either Just (const Nothing)) rawE
  txtD <- validatedTextField lbl (showt v0) mErrsD
  let rawE = updated $ f <$> txtD
  holdDyn v0 $ fmapMaybe (either (const Nothing) Just) rawE

displayError :: (MonadFrontBase t m, LocalizedPrint l) => Dynamic t l -> m ()
displayError errD = do
  langD <- getLanguage
  let localizedErrD = zipDynWith localizedShow langD errD
  dynText localizedErrD
  br

passField :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> m (Dynamic t Text)
passField lbl = fmap _inputElement_value $ labeledTextInput lbl $ def
  & inputElementConfig_elementConfig . elementConfig_initialAttributes %~ ((<>) ("type" =: "password"))

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
      inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        (  "id"          =: i
        <> "class"       =: "eyed-field"
        <> "name"        =: ("password-" <> i)
        <> "placeholder" =: "******"
        <> "type"        =: initType
        ))
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
textInputTypeDyn :: forall t m . MonadFrontBase t m => Event t Text -> InputElementConfig EventResult t (DomBuilderSpace m) -> m (Dynamic t Text)
textInputTypeDyn typeE cfg = fmap _inputElement_value $ inputElement $ cfg
  & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap ((=:) "type" . Just) typeE
