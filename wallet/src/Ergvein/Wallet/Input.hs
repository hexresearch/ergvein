module Ergvein.Wallet.Input(
    Password
  , textField
  , passField
  , passFieldWithEye
  , submitClass
  , textInputTypeDyn
  ) where

import Control.Monad (join)
import Data.Text (Text)
import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Embed
import Ergvein.Wallet.Embed.TH
import Ergvein.Wallet.Id
import Ergvein.Wallet.Monad
import Reflex.Dom

type Password = Text

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

-- | Password field with toggleable visibility
passFieldWithEye :: MonadFront t m => Text -> m (Dynamic t Password)
passFieldWithEye lbl = mdo
  i <- genId
  label i $ text lbl
  let initType = "password"
  typeD <- holdDyn initType $ poke eyeE $ const $ do
    v <- sampleDyn typeD
    pure $ if v == "password" then "text" else "password"
  valD <- textInputTypeDyn (updated typeD) (def &
      textInputConfig_attributes .~ pure
        (  "id"          =: showt i
        <> "class"       =: "eyed-field"
        <> "name"        =: ("password-" <> showt i)
        <> "placeholder" =: "******"
        )
      & textInputConfig_inputType .~ initType)
  smallEyeUrl <- createObjectURL smallEye
  eyeE <- divButton "small-eye" $ imgClass smallEyeUrl ""
  pure valD

-- | Form submit button
submitClass :: MonadFront t m => Dynamic t Text -> Dynamic t Text -> m (Event t ())
submitClass classD lblD = do
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
textInputTypeDyn :: forall t m . MonadFront t m => Event t Text -> TextInputConfig t -> m (Dynamic t Text)
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
