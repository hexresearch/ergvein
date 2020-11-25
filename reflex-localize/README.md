# reflex-localize

Library provides helpers for dynamic strings that depends on current selected
language.

# How to use

First, you should define which languages your app supports:
``` haskell
module App.Language(
    Language(..)
  , module Reflex.Localize
  ) where

import Reflex.Localize
import Reflex.Localize.Language

data instance Language
  = English
  | Russian
```

Second, define enumeration for strings ids.

``` haskell
module App.Localization(
    module App.Localization
  , module App.Language
  ) where

import App.Language

data AboutPageStrings =
    AboutTitle
  | AboutVersion
  | AboutLicence
  | AboutHomepage
  | AboutDevelopers

instance LocalizedPrint AboutPageStrings where
  localizedShow l v = case l of
    English -> case v of
      AboutTitle      -> "About"
      AboutVersion    -> "Version"
      AboutLicence    -> "Licence"
      AboutHomepage   -> "Homepage"
      AboutDevelopers -> "Developers"
    Russian -> case v of
      AboutTitle      -> "О продукте"
      AboutVersion    -> "Версия"
      AboutLicence    -> "Лицензия"
      AboutHomepage   -> "Сайт"
      AboutDevelopers -> "Разработчики"
```

You can either collect all strings to one data sum or split strings for each
widget.

Finally, you should implement `MonadLocalized` type class in you application monad.
We suggest using monad transformer `LocalizeT` via `runLocalize` function:

``` haskell
runLocalize :: (Reflex t, TriggerEvent t m, MonadIO m) => Language -> LocalizeT t m a -> m a
```
