module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import qualified Data.Map.Strict as M
import Reflex.Dom

import Ergvein.Text
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper

data SubPageSettings
  = GoLanguage
  | GoPinCode
  | GoUnits

settingsPage :: MonadFront t m => m ()
settingsPage = do
  let thisWidget = Just $ pure $ settingsPage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    divClass "initial-options grid1" $ do
      goLangE   <- fmap (GoLanguage <$) $ outlineButton STPSButLanguage
      goPinE    <- fmap (GoPinCode  <$) $ outlineButton STPSButPinCode
      goUnitsE  <- fmap (GoUnits    <$) $ outlineButton STPSButUnits
      let goE = leftmost [goLangE, goPinE, goUnitsE]
      void $ nextWidget $ ffor goE $ \spg -> Retractable {
          retractableNext = case spg of
            GoLanguage  -> languagePage
            GoPinCode   -> pinCodePage
            GoUnits     -> unitsPage
        , retractablePrev = thisWidget
        }

languagePage :: MonadFront t m => m ()
languagePage = do
  let thisWidget = Just $ pure $ languagePage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSelectLanguage
    divClass "initial-options grid1" $ do
      langD <- getLanguage
      initKey <- sample . current $ langD
      let listLangsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allLanguages
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated langD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- dropdown initKey listLangsD ddnCfg
      let selD = _dropdown_value dp
      selE <- fmap updated $ holdUniqDyn selD
      widgetHold (pure ()) $ setLanguage <$> selE
      settings <- getSettings
      updateSettings $ ffor selE (\lng -> settings {settingsLang = lng})
      pure ()
    pure ()

pinCodePage :: MonadFront t m => m ()
pinCodePage = do
  let thisWidget = Just $ pure $ pinCodePage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSetsPinCode
    divClass "initial-options grid1" $ do
      pure ()
    pure ()

data Testw = TestOne | TestTwo

type family Units a where
  Units TestOne = Bool
  Units TestTwo = Int

data UnitsBTC
  = U_BTC
  | U_mBTC
  | U_uBTC
  | U_satoshi
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance LocalizedPrint UnitsBTC where
  localizedShow _ v = case v of
    U_BTC     -> "BTC"
    U_mBTC    -> "mBTC"
    U_uBTC    -> "uBTC"
    U_satoshi -> "satoshi"

allUnitsBTC :: [UnitsBTC]
allUnitsBTC = [minBound .. maxBound]

unitsPage :: MonadFront t m => m ()
unitsPage = do
  let thisWidget = Just $ pure $ unitsPage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSelectUnitsBTC
    divClass "initial-options grid1" $ do
      langD <- getLanguage
      let unitBtcD = constDyn U_BTC
      initKey <- sample . current $ unitBtcD
      let listUnitsBtcD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allUnitsBTC
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated unitBtcD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- dropdown initKey listUnitsBtcD ddnCfg
      let selD = _dropdown_value dp
      selE <- fmap updated $ holdUniqDyn selD
      --widgetHold (pure ()) $ setLanguage <$> selE
      --settings <- getSettings
      --updateSettings $ ffor selE (\lng -> settings {settingsLang = lng})
      pure ()
    pure ()
