module App.Localization(
    module App.Localization
  , module App.Language
  ) where

import App.Language

data AppStrings =
    ButtonLabel
  | PressedLabel

instance LocalizedPrint AppStrings where
  localizedShow l v = case l of
    English -> case v of
      ButtonLabel     -> "Button"
      PressedLabel    -> "Button is pressed!"
    Russian -> case v of
      ButtonLabel     -> "Кнопка"
      PressedLabel    -> "Кнопка нажата!"
