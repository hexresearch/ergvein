module Sepulcas.Elements.Input(
    textInput -- +
  , labeledTextInput -- +
  , textFieldTemplate -- +
  , labeledTextFieldTemplate -- +
  , textField -- +
  , labeledTextField -- +
  , displayErrors -- +
  , textFieldWithBtns -- +
  , labeledTextFieldWithBtns -- +
  , labeledTextFieldWithBtnsAndSelector -- +
  , passField
  , Inputable(..)
  , valueField
  , submitClass
  , textInputTypeDyn
  , displayErrorDyn
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Either (fromLeft)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Data.Tuple.Select (sel1)
import Reflex.Dom hiding (textInput)
import Reflex.Flunky
import Reflex.Localize
import Reflex.Localize.Dom
import Sepulcas.Either
import Sepulcas.Elements.Button
import Sepulcas.Elements.Form
import Sepulcas.Elements.Input.Class
import Sepulcas.Elements.Markup
import Sepulcas.Elements.Table
import Sepulcas.Id
import Sepulcas.Monad
import Sepulcas.Native
import Sepulcas.Text

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Sepulcas.Validate (ValidationError)

padTextLeft :: Text -> Text
padTextLeft t = if T.null t then t else " " <> t

-- | If the ID is specified in the config, returns it, otherwise generates a new one.
getElementId :: MonadReflex t m => InputElementConfig EventResult t (DomBuilderSpace m) -> m Text
getElementId cfg =
  let attributes = cfg ^. inputElementConfig_elementConfig . elementConfig_initialAttributes
  in maybe genId pure (M.lookup "id" attributes)

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
  inputId <- getElementId cfg
  label inputId $ localizedText lbl
  inputElement $ cfg
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
      %~ \attributes -> if "type" `M.member` attributes
        then "id" =: inputId <> attributes
        else "id" =: inputId <> "type" =: "text" <> attributes

textFieldTemplate :: MonadReflex t m
  => Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> Event t Text -- ^ Event that updates input value
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> m (Dynamic t Text)
textFieldTemplate value attributes setValueE modifyAttributesE = fmap _inputElement_value $ textInput $ def
  & inputElementConfig_initialValue .~ value
  & inputElementConfig_setValue .~ setValueE
  & inputElementConfig_elementConfig . elementConfig_initialAttributes
    %~ (<> attributes)
  & inputElementConfig_elementConfig . elementConfig_modifyAttributes
    .~ modifyAttributesE

labeledTextFieldTemplate :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
  => l -- ^ Label
  -> Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> Event t Text -- ^ Event that updates input value
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> m (Dynamic t Text)
labeledTextFieldTemplate lbl value attributes setValueE modifyAttributesE = fmap _inputElement_value $ labeledTextInput lbl $ def
    & inputElementConfig_initialValue .~ value
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttributesE
    & inputElementConfig_setValue .~ setValueE

displayErrorDyn :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m) => Dynamic t l -> m ()
displayErrorDyn errD = do
  langD <- getLanguage
  let localizedErrD = zipDynWith localizedShow langD errD
  dynText localizedErrD
  br

displayErrors :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m) => [l] -> m ()
displayErrors errs = case errs of
  [] -> pure ()
  errors -> divClass "form-field-errors" $ do
    traverse_ (\err -> localizedText err >> br) (init errs)
    localizedText $ last errs

textField :: (MonadReflex t m, LocalizedPrint l, MonadLocalized t m)
  => Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> Event t Text -- ^ Event that updates input value
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> (Text -> Either [l] a) -- ^ Validator
  -> m (Dynamic t Text)
textField value attributes setValueE modifyAttributesE validate = mdo
  let
    errsD = fromLeft [] . validate <$> inputValueD
    validityAttrE = (\errs -> if null errs then "class" =: Nothing else "class" =: Just "is-invalid") <$> updated errsD
    inputField = textFieldTemplate value attributes setValueE (modifyAttributesE <> validityAttrE)
  inputValueD <- inputField
  void $ networkHoldDyn $ ffor errsD displayErrors
  pure inputValueD

labeledTextField :: (MonadReflex t m, LocalizedPrint l0, LocalizedPrint l1, MonadLocalized t m)
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> Event t Text -- ^ Event that updates input value
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> (Text -> Either [l1] a) -- ^ Validator
  -> m (Dynamic t Text)
labeledTextField lbl value attributes setValueE modifyAttributesE validate = mdo
  let
    errsD = fromLeft [] . validate <$> inputValueD
    validityAttrE = (\errs -> if null errs then "class" =: Nothing else "class" =: Just "is-invalid") <$> updated errsD
    inputField = labeledTextFieldTemplate lbl value attributes setValueE (modifyAttributesE <> validityAttrE)
  inputValueD <- inputField
  void $ networkHoldDyn $ ffor errsD displayErrors
  pure inputValueD

textFieldWithBtns ::
  ( MonadReflex t m,
    MonadLocalized t m,
    LocalizedPrint l
  )
  => Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> [m ()] -- ^ Buttons on the right side of the input field
  -> Event t Text -- ^ Event that updates input value
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> (Text -> Either [l] a) -- ^ Validator
  -> m (Dynamic t Text, [Event t ()])
textFieldWithBtns value attributes buttons setValueE modifyAttributesE validate = mdo
  let
    errsD = fromLeft [] . validate <$> fst result
    inputClasses = "text-input-with-btns" <> if isAndroid then "-android" else "-desktop"
    validityAttrE = (\errs -> if null errs
      then "class" =: Just inputClasses
      else "class" =: Just ("is-invalid" <> padTextLeft inputClasses))
        <$> updated errsD
    inputField = divClassDyn "text-input-with-btns-wrapper" $ do
      inputValueD <- textFieldTemplate value ("class" =: inputClasses <> attributes) setValueE (modifyAttributesE <> validityAttrE)
      buttonEvents <- divClass "text-input-btns" $ traverse (divButton "text-input-btn") buttons
      pure (inputValueD, buttonEvents)
  result <- inputField
  void $ networkHoldDyn $ ffor errsD displayErrors
  pure result

labeledTextFieldWithBtns ::
  ( MonadReflex t m,
    MonadLocalized t m,
    LocalizedPrint l0,
    LocalizedPrint l1
  )
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> [m ()] -- ^ Buttons on the right side of the input field
  -> Event t Text -- ^ Event that updates input value
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> (Text -> Either [l1] a) -- ^ Validator
  -> m (Dynamic t Text, [Event t ()])
labeledTextFieldWithBtns lbl value attributes buttons setValueE modifyAttributesE validate = mdo
  let
    errsD = fromLeft [] . validate <$> fst result
    inputClasses = "text-input-with-btns" <> if isAndroid then "-android" else "-desktop"
    validityAttrE = (\errs -> if null errs
      then "class" =: Just inputClasses
      else "class" =: Just ("is-invalid" <> padTextLeft inputClasses))
      <$> updated errsD
    inputField id = divClassDyn "text-input-with-btns-wrapper" $ do
      inputValueD <- textFieldTemplate value ("id" =: id <> "class" =: inputClasses <> attributes) setValueE (modifyAttributesE <> validityAttrE)
      buttonEvents <- divClass "text-input-btns" $ traverse (divButton "text-input-btn") buttons
      pure (inputValueD, buttonEvents)
  inputId <- maybe genId pure (M.lookup "id" attributes)
  label inputId $ localizedText lbl
  result <- inputField inputId
  void $ networkHoldDyn $ ffor errsD displayErrors
  pure result

labeledTextFieldWithBtnsAndSelector :: forall t m l0 l1 a .
  ( MonadReflex t m,
    MonadLocalized t m,
    LocalizedPrint l0,
    LocalizedPrint l1
  )
  => l0 -- ^ Label
  -> Text -- ^ Initial value
  -> M.Map AttributeName Text -- ^ Initial attributes
  -> [m ()] -- ^ Buttons on the right side of the input field
  -> m (Dynamic t a) -- ^ Selector
  -> Event t Text -- ^ Event that updates input value
  -> Event t (M.Map AttributeName (Maybe Text)) -- ^ Event that modifies attributes
  -> (Text -> PushM t (Either [l1] a)) -- ^ Validator
  -> m (Dynamic t Text, [Event t ()], Dynamic t a)
labeledTextFieldWithBtnsAndSelector lbl value attributes buttons selector setValueE modifyAttributesE validate = mdo
  let
    errsD :: Dynamic t [l0] = fromLeft [] . pure . validate <$> sel1 result
    inputClasses = "text-input-with-btns" <> if isAndroid then "-android" else "-desktop"
    validityAttrE = (\errs -> if null errs
      then "class" =: Just inputClasses
      else "class" =: Just ("is-invalid" <> padTextLeft inputClasses))
      <$> updated errsD
    inputField id = divClassDyn "text-input-with-btns-wrapper" $ do
      inputValueD <- textFieldTemplate value ("id" =: id <> "class" =: inputClasses <> attributes) setValueE (modifyAttributesE <> validityAttrE)
      buttonEvents <- divClass "text-input-btns" $ traverse (divButton "text-input-btn") buttons
      pure (inputValueD, buttonEvents)
  inputId <- maybe genId pure (M.lookup "id" attributes)
  label inputId $ localizedText lbl
  result <- row $ do
    (inputD, btnEvents) <- column67 $ inputField inputId
    selectorD <- column33 selector
    pure (inputD, btnEvents, selectorD)
  void $ networkHoldDyn $ ffor errsD displayErrors
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
    Just e -> divClass "form-field-errors" $ displayErrorDyn $ pure e
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
