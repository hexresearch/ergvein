{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Widget.Input.BTC.Amount(
    sendAmountWidget
  ) where

import Control.Monad.Except
import Data.Maybe
import Data.Word

import Ergvein.Either
import Ergvein.Types
import Sepulcas.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Send
import Ergvein.Wallet.Localization.Settings()
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Widget.Balance

import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Input field with units. Converts everything to satoshis and returns the unit
sendAmountWidget :: MonadFront t m => Maybe (UnitBTC, Word64) -> Event t () -> m (Dynamic t (Maybe (UnitBTC, Word64)))
sendAmountWidget minit submitE = divClass "amoun-input" $ mdo
  setUs <- fmap (fromMaybe defUnits . settingsUnits) getSettings
  let (unitInit, txtInit) = maybe (setUs, "") (\(u, a) -> let us = Units (Just u) Nothing
        in (us, showMoneyUnit (Money BTC a) us)) minit
  let errsD = fmap (maybe [] id) amountErrsD
  let isInvalidD = fmap (maybe "" (const "is-invalid")) amountErrsD
  amountValD <- do
    el "label" $ localizedText AmountString
    divClass "row" $ mdo
      textInputValueD <- divClass "column column-67" $ do
        textInputValueD' <- (fmap . fmap) T.unpack $ divClassDyn isInvalidD $ textFieldAttrNoLabel (M.singleton "class" "mb-0") never never txtInit
        when isAndroid (availableBalanceWidget unitD)
        pure textInputValueD'
      unitD <- divClass "column column-33" $ do
        unitsDropdown (getUnitBTC unitInit) allUnitsBTC
      pure $ zipDynWith (\u v -> fmap (u,) $ toEither $ validateBtcWithUnits u v) unitD textInputValueD
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  amountErrsD :: Dynamic t (Maybe [VError ()]) <- holdDyn Nothing $ ffor (current amountValD `tag` submitE) eitherToMaybe'
  pure $ eitherToMaybe <$> amountValD
  where
    availableBalanceWidget uD = do
      balanceValue <- balancesWidget BTC
      balanceText <- localized SendAvailableBalance
      let balanceVal = zipDynWith (\x y -> showMoneyUnit x (Units (Just y) Nothing) <> " " <> btcSymbolUnit y) balanceValue uD
          balanceTxt = zipDynWith (\x y -> x <> ": " <> y) balanceText balanceVal
      divClass "amount-available-balance" $ dynText balanceTxt
    unitsDropdown val allUnits = do
      langD <- getLanguage
      let unitD = constDyn val
      initKey <- sample . current $ unitD
      let listUnitsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allUnits
          ddnCfg = DropdownConfig {
              _dropdownConfig_setValue = updated unitD
            , _dropdownConfig_attributes = constDyn ("class" =: "mb-0")
            }
      dp <- dropdown initKey listUnitsD ddnCfg
      let selD = _dropdown_value dp
      holdUniqDyn selD
    displayError :: (MonadFrontBase t m, LocalizedPrint l) => Dynamic t l -> m ()
    displayError errD = do
      langD <- getLanguage
      let localizedErrD = zipDynWith localizedShow langD errD
      dynText localizedErrD
      br
