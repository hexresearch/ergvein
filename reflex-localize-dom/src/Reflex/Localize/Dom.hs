-- |
-- Module      : Reflex.Localize.Dom
-- Copyright   : (c) 2019-2020 ATUM SOLUTIONS AG
-- License     : MIT
-- Maintainer  : ncrashed@protonmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Dom helpers to show localized
--
module Reflex.Localize.Dom(
    localizedText
  , localizedTextWith
  , localizedTextLower
  , localizedTextUpper
  , localizedDyn
  , localizedDynText
  , languageDropdown
  , module Reflex.Localize
  ) where

import Control.Monad.Fix
import Data.Text (Text)
import Reflex.Dom
import Reflex.Localize
import Reflex.Localize.Language
import Reflex.Localize.Trans

import qualified Data.Text as T
import qualified Data.Map.Strict as M

-- | Simple dropdown that allows to select a language and set it for current context. The dropdown has `select-lang` CSS class.
languageDropdown :: (MonadLocalized t m, MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m, LocalizedPrint Language, Eq Language, Enum Language, Ord Language, Bounded Language) => m ()
languageDropdown = do
  langD <- getLanguage
  initKey <- sample . current $ langD
  let listLangsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) [minBound .. maxBound]
      ddnCfg = DropdownConfig {
            _dropdownConfig_setValue   = updated langD
          , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
          }
  dp <- dropdown initKey listLangsD ddnCfg
  let selD = _dropdown_value dp
  selE <- fmap updated $ holdUniqDyn selD
  widgetHold_ (pure ()) $ setLanguage <$> selE

-- | Same as 'text', but changes when language is changed.
localizedText :: (MonadLocalized t m, LocalizedPrint a, PostBuild t m, DomBuilder t m) => a -> m ()
localizedText val = dynText =<< localized val

-- | Same as `localizedText` but transforms text before displaying.
localizedTextWith :: (MonadLocalized t m, LocalizedPrint a, PostBuild t m, DomBuilder t m) => (Text -> Text) -> a -> m ()
localizedTextWith f val = dynText =<< ((fmap . fmap) f $ localized val)

-- | Same as `localizedText` but transforms text to lower case.
localizedTextLower :: (MonadLocalized t m, LocalizedPrint a, PostBuild t m, DomBuilder t m) => a -> m ()
localizedTextLower = localizedTextWith T.toLower

-- | Same as `localizedText` but transforms text to upper case.
localizedTextUpper :: (MonadLocalized t m, LocalizedPrint a, PostBuild t m, DomBuilder t m) => a -> m ()
localizedTextUpper = localizedTextWith T.toUpper

-- | Same as 'localized', but takes a dynamic value as input
localizedDyn :: (MonadLocalized t m, LocalizedPrint a) => Dynamic t a -> m (Dynamic t Text)
localizedDyn valD = do
  langD <- getLanguage
  pure $ localizedShow <$> langD <*> valD

-- | Same as 'localizedText', but takes a dynamic value as input
localizedDynText :: (MonadLocalized t m, LocalizedPrint a, PostBuild t m, DomBuilder t m) => Dynamic t a -> m ()
localizedDynText valD = dynText =<< localizedDyn valD

deriving instance DomBuilder t m => DomBuilder t (LocalizeT t m)
