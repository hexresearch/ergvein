{-# OPTIONS_GHC -Wall #-}

module Sepulcas.Elements.Input(
    textInput
  , labeledTextInput
  , textField
  , labeledTextField
  , displayErrors
  , textFieldWithBtns
  , labeledTextFieldWithBtns
  , labeledTextFieldWithBtnsAndSelector
  , passField
  , submitClass
  , textInputTypeDyn
  , mkErrsDyn
  , displayErrorDyn
  ) where

import Control.Lens
import Data.Either (fromLeft)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Reflex.Dom hiding (textInput)
import Reflex.Flunky
import Reflex.Localize
import Reflex.Localize.Dom
import Sepulcas.Elements.Button
import Sepulcas.Elements.Form
import Sepulcas.Elements.Markup
import Sepulcas.Elements.Table
import Sepulcas.Id
import Sepulcas.Monad
import Sepulcas.Native

import qualified Data.Map.Strict as M

-- | If the ID is specified in the config, returns it, otherwise generates a new one.
getElementID :: MonadReflex t m => InputElementConfig EventResult t (DomBuilderSpace m) -> m Text
getElementID cfg =
  let attrs = cfg ^. inputElementConfig_elementConfig . elementConfig_initialAttributes
  in maybe genId pure (M.lookup "id" attrs)

textInputElement :: MonadReflex t m
  => InputElementConfig EventResult t (DomBuilderSpace m) -- ^ Element config
  -> m (InputElement EventResult (DomBuilderSpace m) t)
textInputElement cfg = do
  inputElement $ cfg
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
      %~ \attrs -> if "type" `M.member` attrs
        then attrs
        else "type" =: "text" <> attrs

labeledTextInputElement :: (MonadReflex t m, MonadLocalized t m, LocalizedPrint l)
  => l -- ^ Label
  -> InputElementConfig EventResult t (DomBuilderSpace m) -- ^ Element config
  -> m (InputElement EventResult (DomBuilderSpace m) t)
labeledTextInputElement lbl cfg = do
  inputID <- getElementID cfg
  label inputID $ localizedText lbl
  inputElement $ cfg
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
      %~ \attrs -> if "type" `M.member` attrs
        then "id" =: inputID <> attrs
        else "id" =: inputID <> "type" =: "text" <> attrs

createInputFromElement :: MonadReflex t m =>
  (InputElementConfig EventResult t (DomBuilderSpace m) -> m (InputElement EventResult (DomBuilderSpace m) t)) -- ^ Element config to element function
  -> Text -- ^ Initial value
  -> Map AttributeName Text -- ^ Initial attributes
  -> Event t Text -- ^ Event that updates input value
  -> Event t (Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> m (Dynamic t Text)
createInputFromElement mkElement val attrs setValueE modifyAttributesE = fmap _inputElement_value $ mkElement $ def
  & inputElementConfig_initialValue .~ val
  & inputElementConfig_setValue .~ setValueE
  & inputElementConfig_elementConfig . elementConfig_initialAttributes
    %~ (<> attrs)
  & inputElementConfig_elementConfig . elementConfig_modifyAttributes
    .~ modifyAttributesE

textInput :: MonadReflex t m
  => Text -- ^ Initial value
  -> Map AttributeName Text -- ^ Initial attributes
  -> Event t Text -- ^ Event that updates input value
  -> Event t (Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> m (Dynamic t Text)
textInput = createInputFromElement textInputElement

labeledTextInput :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
  => l -- ^ Label
  -> Text -- ^ Initial value
  -> Map AttributeName Text -- ^ Initial attributes
  -> Event t Text -- ^ Event that updates input value
  -> Event t (Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> m (Dynamic t Text)
labeledTextInput lbl = createInputFromElement (labeledTextInputElement lbl)

displayErrorDyn :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m) => Dynamic t l -> m ()
displayErrorDyn errD = do
  langD <- getLanguage
  let localizedErrD = zipDynWith localizedShow langD errD
  dynText localizedErrD
  br

displayErrors :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m) => [l] -> m ()
displayErrors errs = case errs of
  [] -> pure ()
  _ -> divClass "form-field-errors" $ do
    traverse_ (\err -> localizedText err >> br) (init errs)
    localizedText $ last errs

errorsWidget :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m) => Dynamic t [l] -> m ()
errorsWidget errsD = dyn_ $ ffor errsD displayErrors

mkInvalidClassE :: Reflex t => Text -> Dynamic t [a] -> Event t (Map AttributeName (Maybe Text))
mkInvalidClassE inputClass errsD = (\errs -> if null errs
  then "class" =: Just inputClass
  else "class" =: Just (inputClass <> " is-invalid")) <$> updated errsD

textField :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
  => Text -- ^ Initial value
  -> Map AttributeName Text -- ^ Initial attributes. Note: don't specify CSS classes here because they will be overwritten
  -> Event t Text -- ^ Event that updates input value
  -> Event t (Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes. Note: don't specify CSS classes here because they will be overwritten
  -> Dynamic t [l] -- ^ Dynamic with errors
  -> m (Dynamic t Text)
textField val attrs setValueE modifyAttributesE errsD = mdo
  let inputClass = "text-input"
      classAttrE = mkInvalidClassE inputClass errsD
  inputValueD <- textInput val ("class" =: inputClass <> attrs) setValueE (classAttrE <> modifyAttributesE)
  errorsWidget errsD
  pure inputValueD

labeledTextField :: (MonadReflex t m, LocalizedPrint l0, LocalizedPrint l1, MonadLocalized t m)
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> Map AttributeName Text -- ^ Initial attributes. Note: don't specify CSS classes here because they will be overwritten
  -> Event t Text -- ^ Event that updates input value
  -> Event t (Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes. Note: don't specify CSS classes here because they will be overwritten
  -> Dynamic t [l1] -- ^ Dynamic with errors
  -> m (Dynamic t Text)
labeledTextField lbl val attrs setValueE modifyAttributesE errsD = mdo
  let inputClass = "text-input"
      classAttrE = mkInvalidClassE inputClass errsD
  inputValueD <- labeledTextInput lbl val ("class" =: inputClass <> attrs) setValueE (classAttrE <> modifyAttributesE)
  errorsWidget errsD
  pure inputValueD

textFieldWithBtns ::
  ( MonadReflex t m,
    MonadLocalized t m,
    LocalizedPrint l
  )
  => Text -- ^ Initial value
  -> Map AttributeName Text -- ^ Initial attributes. Note: don't specify CSS classes here because they will be overwritten
  -> [m ()] -- ^ Buttons on the right side of the input field
  -> Event t Text -- ^ Event that updates input value
  -> Event t (Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes. Note: don't specify CSS classes here because they will be overwritten
  -> Dynamic t [l] -- ^ Dynamic with errors
  -> m (Dynamic t Text, [Event t ()])
textFieldWithBtns val attrs buttons setValueE modifyAttributesE errsD = mdo
  let inputClass = "text-input-with-btns" <> if isAndroid then "-android" else "-desktop"
      classAttrE = mkInvalidClassE inputClass errsD
  result <- divClassDyn "text-input-with-btns-wrapper" $ do
    inputD <- textInput val ("class" =: inputClass <> attrs) setValueE (classAttrE <> modifyAttributesE)
    btnEvents <- divClass "text-input-btns" $ traverse (divButton "text-input-btn") buttons
    pure (inputD, btnEvents)
  errorsWidget errsD
  pure result

labeledTextFieldWithBtns ::
  ( MonadReflex t m,
    MonadLocalized t m,
    LocalizedPrint l0,
    LocalizedPrint l1
  )
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> Map AttributeName Text -- ^ Initial attributes. Note: don't specify CSS classes here because they will be overwritten
  -> [m ()] -- ^ Buttons on the right side of the input field
  -> Event t Text -- ^ Event that updates input value
  -> Event t (Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes. Note: don't specify CSS classes here because they will be overwritten
  -> Dynamic t [l1] -- ^ Dynamic with errors
  -> m (Dynamic t Text, [Event t ()])
labeledTextFieldWithBtns lbl val attrs buttons setValueE modifyAttributesE errsD = mdo
  let inputClass = "text-input-with-btns" <> if isAndroid then "-android" else "-desktop"
      classAttrE = mkInvalidClassE inputClass errsD
  inputID <- maybe genId pure (M.lookup "id" attrs)
  label inputID $ localizedText lbl
  result <- divClassDyn "text-input-with-btns-wrapper" $ do
    inputD <- textInput val ("id" =: inputID <> "class" =: inputClass <> attrs) setValueE (classAttrE <> modifyAttributesE)
    btnEvents <- divClass "text-input-btns" $ traverse (divButton "text-input-btn") buttons
    pure (inputD, btnEvents)
  errorsWidget errsD
  pure result

labeledTextFieldWithBtnsAndSelector ::
  ( MonadReflex t m,
    MonadLocalized t m,
    LocalizedPrint l0,
    LocalizedPrint l1
  )
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> Map AttributeName Text -- ^ Initial attributes. Note: don't specify CSS classes here because they will be overwritten
  -> [m ()] -- ^ Buttons on the right side of the input field
  -> m (Dynamic t a) -- ^ Selector
  -> Event t Text -- ^ Event that updates input value
  -> Event t (Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes. Note: don't specify CSS classes here because they will be overwritten
  -> Dynamic t [l1] -- ^ Dynamic with errors
  -> m (Dynamic t Text, [Event t ()], Dynamic t a)
labeledTextFieldWithBtnsAndSelector lbl val attrs buttons selector setValueE modifyAttributesE errsD = mdo
  let inputClass = "text-input-with-btns" <> if isAndroid then "-android" else "-desktop"
      classAttrE = mkInvalidClassE inputClass errsD
  inputID <- maybe genId pure (M.lookup "id" attrs)
  label inputID $ localizedText lbl
  result <- row $ do
    (inputD, btnEvents) <- column67 $ divClassDyn "text-input-with-btns-wrapper" $ do
      inputD <- textInput val ("id" =: inputID <> "class" =: inputClass <> attrs) setValueE (classAttrE <> modifyAttributesE)
      btnEvents <- divClass "text-input-btns" $ traverse (divButton "text-input-btn") buttons
      pure (inputD, btnEvents)
    selD <- column33 selector
    pure (inputD, btnEvents, selD)
  errorsWidget errsD
  pure result

-- | Password field with toggleable input visibility
passField :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m) => l -> Event t () -> m (Dynamic t Text)
passField lbl clearInputE = mdo
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

-- | Applies the validator to the dynamic by the submit event, returns the dynamic with a list of validation errors.
mkErrsDyn :: MonadReflex t m
  => Event t () -- ^ Event that triggers validation
  -> Dynamic t Text -- ^ Dynamic with input text
  -> (Text -> PushM t (Either [a] b)) -- ^ Validator
  -> m (Dynamic t [a])
mkErrsDyn submitE inputD validator = holdDyn [] errsE where
  inputE = poke submitE $ const $ sampleDyn inputD
  errsE = fromLeft [] <$> poke inputE validator

-- | Wrapper around text field that allows to switch its type dynamically with
-- saving of previous value.
textInputTypeDyn :: forall t m . MonadReflex t m => Event t Text -> InputElementConfig EventResult t (DomBuilderSpace m) -> m (Dynamic t Text)
textInputTypeDyn typeE cfg = fmap _inputElement_value $ inputElement $ cfg
  & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap ((=:) "type" . Just) typeE
