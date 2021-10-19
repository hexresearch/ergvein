module Sepulcas.Elements.Input(
    textInput
  , labeledTextInput
  -- , validatedTextInput
  -- , textInputValidated
  , textField
  , labeledTextField
  -- , textField
  -- , textFieldAttrNoLabel
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
  , validatedTextFieldWithSetValBtns
  , displayError
  , validatedTextFieldWithSetValTextBtnNoLabel
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Reflex.Dom hiding (textInput)
import Reflex.Flunky
import Reflex.Localize
import Reflex.Localize.Dom
import Sepulcas.Either
import Sepulcas.Elements.Button
import Sepulcas.Elements.Form
import Sepulcas.Elements.Input.Class
import Sepulcas.Elements.Markup
import Sepulcas.Id
import Sepulcas.Monad
import Sepulcas.Native
import Sepulcas.Text

import qualified Data.Map.Strict as M
import qualified Data.Text as T

textInput :: MonadReflex t m
  => InputElementConfig EventResult t (DomBuilderSpace m) -- ^ Element config
  -> m (InputElement EventResult (DomBuilderSpace m) t)
textInput cfg = do
  inputElement $ cfg
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
      %~ \attributes -> if "type" `M.member` attributes
        then attributes
        else "type" =: "text" <> attributes

labeledTextInput :: (MonadReflex t m, MonadLocalized t m, LocalizedPrint l)
  => l -- ^ Label
  -> InputElementConfig EventResult t (DomBuilderSpace m) -- ^ Element config
  -> m (InputElement EventResult (DomBuilderSpace m) t)
labeledTextInput lbl cfg = do
  inputId <- genId
  label inputId $ localizedText lbl
  inputElement $ cfg
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
      %~ \attributes -> if "type" `M.member` attributes
        then "id" =: inputId <> attributes
        else "id" =: inputId <> "type" =: "text" <> attributes

-- validatedTextInput :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
--   => InputElementConfig EventResult t (DomBuilderSpace m)
--   -> Dynamic t (Maybe [l]) -- ^ List of errors
--   -> m (InputElement EventResult (DomBuilderSpace m) t)
-- validatedTextInput cfg mErrsD = do
--   tInput <- inputField
--   void $ divClass "form-field-errors" $ simpleList errsD displayError
--   pure tInput
--   where
--     errsD = fromMaybe [] <$> mErrsD
--     isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
--     inputField = divClassDyn isInvalidD $ textInput cfg

-- textInputValidated :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
--   => InputElementConfig EventResult t (DomBuilderSpace m)
--   -> (Text -> Either [l] a) -- ^ Validator
--   -> m (InputElement EventResult (DomBuilderSpace m) t) -- ^ Only valid values get through
-- textInputValidated cfg f = mdo
--   mErrsD <- holdDyn Nothing $ fmap justLeft rawE
--   ti <- validatedTextInput cfg mErrsD
--   let txtD = _inputElement_value ti
--       rawE = updated $ f <$> txtD
--   pure ti

-- textField :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
--   => l -- ^ Label
--   -> Text -- ^ Initial value
--   -> m (Dynamic t Text)
-- labeledTextField lbl value = fmap _inputElement_value $ labeledTextInput lbl M.empty $ def
--   & inputElementConfig_initialValue .~ value

textField :: MonadReflex t m
  => Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> Event t Text -- ^ Event that updates input value
  -> m (Dynamic t Text)
textField value attributes modifyAttributesE setValueE = fmap _inputElement_value $ textInput $ def
  & inputElementConfig_initialValue .~ value
  & inputElementConfig_setValue .~ setValueE
  & inputElementConfig_elementConfig . elementConfig_initialAttributes
    %~ (<> attributes)
  & inputElementConfig_elementConfig . elementConfig_modifyAttributes
    .~ modifyAttributesE

labeledTextField :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
  => l -- ^ Label
  -> Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> Event t Text -- ^ Event that updates input value
  -> m (Dynamic t Text)
labeledTextField lbl value attributes modifyAttributesE setValueE = fmap _inputElement_value $ labeledTextInput lbl $ def
    & inputElementConfig_initialValue .~ value
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttributesE
    & inputElementConfig_setValue .~ setValueE

validatedTextField :: (MonadReflex t m, LocalizedPrint l0, LocalizedPrint l1, MonadLocalized t m)
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> m (Dynamic t Text)
validatedTextField lbl value mErrsD = do
  textInputValueD <- inputField
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  pure textInputValueD
  where
    errsD = fromMaybe [] <$> mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ labeledTextField lbl value M.empty never never

validatedTextFieldSetVal :: (MonadReflex t m, LocalizedPrint l0, LocalizedPrint l1, MonadLocalized t m)
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> Event t Text
  -> m (Dynamic t Text)
validatedTextFieldSetVal lbl value mErrsD setValueE = do
  textInputValueD <- inputField
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  pure textInputValueD
  where
    errsD = fromMaybe [] <$> mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ fmap _inputElement_value $ labeledTextInput lbl $ def
      & inputElementConfig_initialValue .~ value
      & inputElementConfig_setValue .~ setValueE

validatedTextFieldSetValNoLabel :: (MonadReflex t m, LocalizedPrint l1, MonadLocalized t m)
  => Text -- ^ Initial value
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> Event t Text
  -> m (Dynamic t Text)
validatedTextFieldSetValNoLabel value mErrsD setValueE = do
  textInputValueD <- inputField
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  pure textInputValueD
  where
    errsD = fromMaybe [] <$> mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ fmap _inputElement_value $ textInput $ def
      & inputElementConfig_initialValue .~ value
      & inputElementConfig_setValue .~ setValueE

validatedTextFieldAttrSetValNoLabel :: (MonadReflex t m, LocalizedPrint l1, MonadLocalized t m)
  => Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> Event t Text -- ^ Event that changes value
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> m (Dynamic t Text)
validatedTextFieldAttrSetValNoLabel initValue initAttrs setValueE modifyAttrsE mErrsD = do
  textInputValueD <- inputField
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  pure textInputValueD
  where
    errsD = fromMaybe [] <$> mErrsD
    isInvalidD = fmap (maybe "" (const "is-invalid")) mErrsD
    inputField = divClassDyn isInvalidD $ fmap _inputElement_value $ textInput $ def
      & inputElementConfig_initialValue .~ initValue
      & inputElementConfig_elementConfig . elementConfig_initialAttributes %~ (\as -> "type" =: "text" <> initAttrs <> as)
      & inputElementConfig_setValue .~ setValueE
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrsE

textFieldSetValValidated :: (MonadReflex t m, LocalizedPrint l0, LocalizedPrint l1, Show a, MonadLocalized t m)
  => l0 -- ^ Label
  -> a -- ^ Initial value
  -> Event t a -- ^ Set value event
  -> (Text -> Either [l1] a) -- ^ Validatior
  -> m (Dynamic t a) -- ^ Only valid values get through
textFieldSetValValidated lbl value setValueE f = mdo
  mErrsD <- holdDyn Nothing $ fmap justLeft rawE
  txtD <- validatedTextFieldSetVal lbl (showt value) mErrsD (showt <$> setValueE)
  let rawE = updated $ f <$> txtD
  holdDyn value $ fmapMaybe justRight rawE

textFieldValidated :: (MonadReflex t m, LocalizedPrint l0, LocalizedPrint l1, Show a, MonadLocalized t m)
  => l0 -- ^ Label
  -> a -- ^ Initial value
  -> (Text -> Either [l1] a) -- ^ Validatior
  -> m (Dynamic t a) -- ^ Only valid values get through
textFieldValidated lbl value f = mdo
  mErrsD <- holdDyn Nothing $ fmap justLeft rawE
  txtD <- validatedTextField lbl (showt value) mErrsD
  let rawE = updated $ f <$> txtD
  holdDyn value $ fmapMaybe justRight rawE

displayError :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m) => Dynamic t l -> m ()
displayError errD = do
  langD <- getLanguage
  let localizedErrD = zipDynWith localizedShow langD errD
  dynText localizedErrD
  br

passField :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
  => l -- ^ Label
  -> m (Dynamic t Text)
passField lbl = fmap _inputElement_value $ labeledTextInput lbl $ def
  & inputElementConfig_elementConfig . elementConfig_initialAttributes %~ (\as -> "type" =: "password" <> as)

-- | Password field with toggleable visibility
passFieldWithEye :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m) => l -> Event t () -> m (Dynamic t Text)
passFieldWithEye lbl clearInputE = mdo
  i <- genId
  label i $ localizedText lbl
  let initType = "password"
  typeD <- holdDyn initType $ poke eyeE $ const $ do
    v <- sampleDyn typeD
    pure $ if v == "password" then "text" else "password"
  (valD, eyeE) <- divClass "password-field" $ do
    valD' <- textInputTypeDyn (updated typeD) (def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        (  "id"          =: i
        <> "class"       =: "eyed-field"
        <> "name"        =: ("password-" <> i)
        <> "placeholder" =: "******"
        <> "type"        =: initType
        )
      & inputElementConfig_setValue .~ ("" <$ clearInputE))
    passwordVisibleD <- toggle False eyeE
    let eyeButtonIconClassD = eyeButtonIconClass <$> passwordVisibleD
    eyeE' <- divButton "small-eye" $ elClassDyn "i" eyeButtonIconClassD blank
    pure (valD', eyeE')
  pure valD

-- | Labeled text input that parses text into given type
valueField :: forall a t m l .(MonadReflex t m, Inputable l a, MonadLocalized t m, Eq a, Show l, Show a)
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
      in labeledTextInput lbl def {
        _inputElementConfig_setValue = Just $ disp <$> updated avD
      , _inputElementConfig_initialValue = disp av0
      }
  void $ networkHoldDyn $ ffor errorD $ \case
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
submitClass :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m) => Dynamic t Text -> l -> m (Event t ())
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
textInputTypeDyn :: forall t m . MonadReflex t m => Event t Text -> InputElementConfig EventResult t (DomBuilderSpace m) -> m (Dynamic t Text)
textInputTypeDyn typeE cfg = fmap _inputElement_value $ inputElement $ cfg
  & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap ((=:) "type" . Just) typeE

validatedTextFieldWithSetValBtns :: (MonadReflex t m, LocalizedPrint l0, LocalizedPrint l1, MonadLocalized t m)
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> Dynamic t (Maybe [l1]) -- ^ List of errors
  -> [Text] -- ^ Classes of icon-buttons
  -> Event t Text -- ^ Event that may change the input field value
  -> m (Dynamic t Text, [Event t ()])
validatedTextFieldWithSetValBtns lbl value mErrsD btns setValueE = mdo
  i <- genId
  label i $ localizedText lbl
  (iD, bE) <- divClassDyn isInvalidD $ do
    textInputValueD <- inputField
    btnEvents <- divClass "text-input-btns" $ traverse mkBtn btns
    pure (textInputValueD, btnEvents)
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  pure (iD, bE)
  where
    errsD = fmap (fromMaybe []) mErrsD
    inputClass = "text-input-with-btns" <> if isAndroid then "-android" else "-desktop"
    isInvalidD = fmap (maybe "text-input-with-btns-wrapper" (const "text-input-with-btns-wrapper is-invalid")) mErrsD
    mkBtn iconClass = divButton "text-input-btn" $ elClass "i" iconClass blank
    inputField = fmap _inputElement_value $ textInput $ def
      & inputElementConfig_initialValue .~ value
      & inputElementConfig_setValue .~ setValueE
      & inputElementConfig_elementConfig . elementConfig_initialAttributes %~ (\as -> "class" =: inputClass <> as)

validatedTextFieldWithSetValTextBtnNoLabel :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
  => Text -- ^ CSS classes
  -> l -- ^ Button text
  -> Event t Text -- ^ Event that may change the input field value
  -> Text -- ^ Initial value
  -> Dynamic t Text -- ^ Dynamic that contains info about errors
  -> m (Dynamic t Text, Event t ())
validatedTextFieldWithSetValTextBtnNoLabel classes btnText setValueE value invalidClassesD = mdo
  (iD, bE) <- divClassDyn (wrapperClasses invalidClassesD) $ do
    textInputValueD <- inputField
    btnE <- divClass "text-input-btns" $ divButton "text-input-btn" $ localizedText btnText
    pure (textInputValueD, btnE)
  pure (iD, bE)
  where
    inputClass = "text-input-with-btns" <> if isAndroid then "-android" else "-desktop" <> padClasses classes
    padClasses c = if T.null c then c else " " <> c
    wrapperClasses invClassesD = (\cls -> "text-input-with-btns-wrapper" <> padClasses cls) <$> invClassesD
    inputField = fmap _inputElement_value $ textInput $ def
      & inputElementConfig_initialValue .~ value
      & inputElementConfig_setValue .~ setValueE
      & inputElementConfig_elementConfig . elementConfig_initialAttributes %~ (\as -> "class" =: inputClass <> as)
