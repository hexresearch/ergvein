module Ergvein.Wallet.Elements.Input(
    Password
  , textInput
  , validatedTextInput
  , textInputValidated
  , textField
  , textFieldAttr
  , textFieldNoLabel
  , textFieldAttrNoLabel
  , validatedTextField
  , validatedTextFieldSetVal
  , validatedTextFieldSetValNoLabel
  , validatedTextFieldAttrSetValNoLabel
  , textFieldSetValValidated
  , textFieldValidated
  , passField
  , passFieldWithEye
  , Inputable(..)
  , valueField
  , submitClass
  , textInputTypeDyn
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Proxy
import Data.Text (Text)
import Ergvein.Either
import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Elements.Input.Class
import Ergvein.Wallet.Id
import Ergvein.Wallet.Monad
import Reflex.Localize
import Reflex.Localize.Dom

import qualified Data.Map.Strict as M

textInput :: MonadFrontBase t m
  => InputElementConfig EventResult t (DomBuilderSpace m)
  -> m (InputElement EventResult (DomBuilderSpace m) t)
textInput cfg = do
  i <- genId
  inputElement $ cfg
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
      %~ (\as -> "id" =: i <> "type" =: "text" <> as)

validatedTextInput :: (MonadFrontBase t m, LocalizedPrint l)
  => InputElementConfig EventResult t (DomBuilderSpace m)
  -> Dynamic t (Maybe [l]) -- ^ List of errors
  -> m (InputElement EventResult (DomBuilderSpace m) t)
validatedTextInput cfg mErrsD = do
  tInput <- inputField
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  pure tInput
  where
    errsD = fmap (maybe [] id) mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ textInput cfg

textInputValidated :: (MonadFrontBase t m, LocalizedPrint l)
  => InputElementConfig EventResult t (DomBuilderSpace m)
  -> (Text -> Either [l] a) -- ^ Validator
  -> m (InputElement EventResult (DomBuilderSpace m) t) -- ^ Only valid values get through
textInputValidated cfg f = mdo
  mErrsD <- holdDyn Nothing $ fmap eitherToMaybe' rawE
  ti <- validatedTextInput cfg mErrsD
  let txtD = _inputElement_value ti
      rawE = updated $ f <$> txtD
  pure ti

labeledTextInput :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> M.Map AttributeName Text
  -> InputElementConfig EventResult t (DomBuilderSpace m)
  -> m (InputElement EventResult (DomBuilderSpace m) t)
labeledTextInput lbl attrs cfg = do
  i <- genId
  label i $ localizedText lbl
  inputElement $ cfg
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
      %~ \as -> "id" =: i <> "type" =: "text" <> attrs <> as

textField :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> Text -- ^ Initial value
  -> m (Dynamic t Text)
textField lbl v0 = fmap _inputElement_value $ labeledTextInput lbl M.empty $ def
  & inputElementConfig_initialValue .~ v0

textFieldAttr :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> M.Map AttributeName Text
  -> Text -- ^ Initial value
  -> m (Dynamic t Text)
textFieldAttr lbl attrs v0 = fmap _inputElement_value $ labeledTextInput lbl attrs $ def
  & inputElementConfig_initialValue .~ v0

textFieldNoLabel :: (MonadFrontBase t m)
  => Text -- ^ Initial value
  -> m (Dynamic t Text)
textFieldNoLabel v0 = fmap _inputElement_value $ inputElement $ def
  & inputElementConfig_initialValue .~ v0
  & inputElementConfig_elementConfig . elementConfig_initialAttributes
    %~ (\as -> "type" =: "text" <> as)

textFieldAttrNoLabel :: (MonadFrontBase t m)
  => M.Map AttributeName Text -- ^ Initial attributes
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> Event t Text
  -> Text -- ^ Initial value
  -> m (Dynamic t Text)
textFieldAttrNoLabel attrs modifyAttrsE setValE v0 = fmap _inputElement_value $ inputElement $ def
  & inputElementConfig_initialValue .~ v0
  & inputElementConfig_elementConfig . elementConfig_initialAttributes
    %~ (\as -> "type" =: "text" <> attrs <> as)
  & inputElementConfig_elementConfig . elementConfig_modifyAttributes
    .~ modifyAttrsE
  & inputElementConfig_setValue .~ setValE

validatedTextField :: (MonadFrontBase t m, LocalizedPrint l0, LocalizedPrint l1)
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> m (Dynamic t Text)
validatedTextField lbl v0 mErrsD = do
  textInputValueD <- inputField
  void $ divClass "form-field-errors" $ simpleList errsD displayError
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
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  pure textInputValueD
  where
    errsD = fmap (maybe [] id) mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ fmap _inputElement_value $ labeledTextInput lbl M.empty $ def
      & inputElementConfig_initialValue .~ v0
      & inputElementConfig_setValue .~ setValE

validatedTextFieldSetValNoLabel :: (MonadFrontBase t m, LocalizedPrint l1)
  => Text -- ^ Initial value
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> Event t Text
  -> m (Dynamic t Text)
validatedTextFieldSetValNoLabel v0 mErrsD setValE = do
  textInputValueD <- inputField
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  pure textInputValueD
  where
    errsD = fmap (maybe [] id) mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ fmap _inputElement_value $ textInput $ def
      & inputElementConfig_initialValue .~ v0
      & inputElementConfig_setValue .~ setValE

validatedTextFieldAttrSetValNoLabel :: (MonadFrontBase t m, LocalizedPrint l1)
  => Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> Event t Text -- ^ Event that changes value
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> m (Dynamic t Text)
validatedTextFieldAttrSetValNoLabel initValue initAttrs setValE modifyAttrsE mErrsD = do
  textInputValueD <- inputField
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  pure textInputValueD
  where
    errsD = fmap (maybe [] id) mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ fmap _inputElement_value $ textInput $ def
      & inputElementConfig_initialValue .~ initValue
      & inputElementConfig_elementConfig . elementConfig_initialAttributes %~ (\as -> "type" =: "text" <> initAttrs <> as)
      & inputElementConfig_setValue .~ setValE
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrsE

textFieldSetValValidated :: (MonadFrontBase t m, LocalizedPrint l0, LocalizedPrint l1, Show a)
  => l0 -- ^ Label
  -> a -- ^ Initial value
  -> Event t a -- ^ Set value event
  -> (Text -> Either [l1] a) -- ^ Validatior
  -> m (Dynamic t a) -- ^ Only valid values get through
textFieldSetValValidated lbl v0 setValE f = mdo
  mErrsD <- holdDyn Nothing $ fmap eitherToMaybe' rawE
  txtD <- validatedTextFieldSetVal lbl (showt v0) mErrsD (showt <$> setValE)
  let rawE = updated $ f <$> txtD
  holdDyn v0 $ fmapMaybe eitherToMaybe rawE

textFieldValidated :: (MonadFrontBase t m, LocalizedPrint l0, LocalizedPrint l1, Show a)
  => l0 -- ^ Label
  -> a -- ^ Initial value
  -> (Text -> Either [l1] a) -- ^ Validatior
  -> m (Dynamic t a) -- ^ Only valid values get through
textFieldValidated lbl v0 f = mdo
  mErrsD <- holdDyn Nothing $ fmap eitherToMaybe' rawE
  txtD <- validatedTextField lbl (showt v0) mErrsD
  let rawE = updated $ f <$> txtD
  holdDyn v0 $ fmapMaybe eitherToMaybe rawE

displayError :: (MonadFrontBase t m, LocalizedPrint l) => Dynamic t l -> m ()
displayError errD = do
  langD <- getLanguage
  let localizedErrD = zipDynWith localizedShow langD errD
  dynText localizedErrD
  br

passField :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> m (Dynamic t Text)
passField lbl = fmap _inputElement_value $ labeledTextInput lbl ("type" =: "password") def

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
    valD' <- textInputTypeDyn (updated typeD) (def &
      inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        (  "id"          =: i
        <> "class"       =: "eyed-field"
        <> "name"        =: ("password-" <> i)
        <> "placeholder" =: "******"
        <> "type"        =: initType
        ))
    passwordVisibleD <- toggle False eyeE
    let eyeButtonIconClassD = eyeButtonIconClass <$> passwordVisibleD
    eyeE' <- divButton "small-eye" $ elClassDyn "i" eyeButtonIconClassD blank
    pure (valD', eyeE')
  pure valD

-- | Labeled text input that parses text into given type
valueField :: forall a t m l .(MonadFrontBase t m, Inputable l a, Eq a, Show l, Show a)
  => l -- ^ Label
  -> Dynamic t a -- ^ Initial value and updates
  -> m (Dynamic t a)
valueField lbl av0D = mdo
  av0 <- sample . current $ av0D
  avD :: Dynamic t a <- holdUniqDyn =<< mergeDyn av0D valE
  errorD <- holdDyn Nothing $ leftmost [Just <$> errorE, Nothing <$ valE]
  let isInvalidD = fmap (maybe "" (const "is-invalid")) errorD
  tInput <- divClassDyn isInvalidD $ let
      disp = displayInput (Proxy :: Proxy l)
      in labeledTextInput lbl mempty def {
        _inputElementConfig_setValue = Just $ disp <$> updated avD
      , _inputElementConfig_initialValue = disp av0
      }
  void $ widgetHoldDyn $ ffor errorD $ \case
    Nothing -> pure ()
    Just e -> divClass "form-field-errors" $ displayError $ pure e
  let resD = parseInput <$> _inputElement_value tInput
      (errorE :: Event t l, valE) = splitEither $ updated resD
  performEvent_ $ ffor (updated resD) $ liftIO . print
  holdUniqDyn =<< holdDyn av0 valE

eyeButtonIconClass :: Bool -> Text
eyeButtonIconClass True = "far fa-eye-slash fa-fw"
eyeButtonIconClass _ = "far fa-eye fa-fw"

-- | Form submit button
submitClass :: (MonadFrontBase t m, LocalizedPrint l) => Dynamic t Text -> l -> m (Event t ())
submitClass classD lbl = do
  lblD <- localized lbl
  let classesD = do
        classVal <- classD
        lbl' <- lblD
        pure $ "class" =: classVal
            <> "type"  =: "submit"
            <> "value" =: lbl'
            <> "onclick" =: "return false;"
  (e, _) <- elDynAttr' "input" classesD blank
  return $ domEvent Click e

-- | Wrapper around text field that allows to switch its type dynamically with
-- saving of previous value.
textInputTypeDyn :: forall t m . MonadFrontBase t m => Event t Text -> InputElementConfig EventResult t (DomBuilderSpace m) -> m (Dynamic t Text)
textInputTypeDyn typeE cfg = fmap _inputElement_value $ inputElement $ cfg
  & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap ((=:) "type" . Just) typeE
