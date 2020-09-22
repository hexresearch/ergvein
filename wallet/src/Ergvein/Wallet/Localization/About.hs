module Ergvein.Wallet.Localization.About(
    AboutPageStrings(..)
  ) where

import Ergvein.Wallet.Language

data AboutPageStrings =
    AboutTitle
  | AboutVersion
  | AboutLicence
  | AboutHomepage
  | AboutDevelopers
  | AboutDistrib

instance LocalizedPrint AboutPageStrings where
  localizedShow l v = case l of
    English -> case v of
      AboutTitle      -> "About Ergvein"
      AboutVersion    -> "Version"
      AboutLicence    -> "Licence"
      AboutHomepage   -> "Homepage"
      AboutDevelopers -> "Developers"
      AboutDistrib    -> ""
    Russian -> case v of
      AboutTitle      -> "О Ergvein"
      AboutVersion    -> "Версия"
      AboutLicence    -> "Лицензия"
      AboutHomepage   -> "Сайт"
      AboutDevelopers -> "Разработчики"
      AboutDistrib    -> ""
