{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import App.Localization
import Data.Map.Strict (Map)
import Data.Text (Text)
import Reflex.Dom
import Reflex.Localize.Dom
import Text.RawString.QQ

main :: IO ()
main = mainWidgetWithCss css $ runLocalize English $ do
  languageDropdown
  pressE <- buttonClass "outline" ButtonLabel
  widgetHold_ (pure ()) $ localizedText PressedLabel <$ pressE
  where
    css = [r|
      select {
        background-color: white;
        color: black;
        font-size: 14pt;
      }
      .outline {
        border-width: 1px;
        border-radius: 10px;
        min-width: 100px;
        min-height: 50px;
        margin-right: 30px;
        color: black;
        background-color: white;
        border-color: black;
        font-size: 14pt;
      }
    |]

buttonClass :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> lbl -> m (Event t ())
buttonClass classValD lbl = mkButton "button" [("onclick", "return false;")] classValD . dynText =<< localized lbl

mkButton :: (DomBuilder t m, PostBuild t m) => Text -> Map Text Text -> Dynamic t Text -> m a -> m (Event t a)
mkButton eltp attrs classValD ma = do
  let classesD = do
        classVal <- classValD
        pure $ attrs <> [("class", classVal)]
  (e, a) <- elDynAttr' eltp classesD ma
  return $ a <$ domEvent Click e
