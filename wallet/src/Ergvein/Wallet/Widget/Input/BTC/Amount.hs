{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Widget.Input.BTC.Amount(
    sendAmountWidget
  ) where

import Control.Monad.Except (when, void)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)

import Ergvein.Core.Transaction.Btc.Fee
import Ergvein.Either
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Widget.Balance (balancesWidget)
import Sepulcas.Elements

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Network.Haskoin.Address as HA
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Transaction as HT

-- | Input field with units. Converts everything to satoshis and returns the unit
sendAmountWidget ::
  MonadFront t m =>
  Dynamic t (Maybe BtcAddress) ->
  Maybe (UnitBTC, Word64) ->
  Event t () ->
  m (Dynamic t (Maybe (UnitBTC, Word64)))
sendAmountWidget mRecipientD minit submitE = divClass "amoun-input" $ mdo
  setUs <- fmap (fromMaybe defUnits . settingsUnits) getSettings
  let (unitInit, txtInit) = maybe (setUs, "") (\(u, a) -> let us = Units (Just u) Nothing
        in (us, showMoneyUnit (Money BTC a) us)) minit
      errsD = fmap (fromMaybe []) amountErrsD
      isInvalidD = fmap (maybe "" (const "is-invalid")) amountErrsD
      addressToDustTreshold address = fromIntegral $ getDustThreshold $ HT.TxOut 0 $ HS.encodeOutputBS $ HA.addressToOutput address
      dustThresholdD = fmap (maybe 0 addressToDustTreshold) mRecipientD
  amountValD <- do
    el "label" $ localizedText AmountString
    divClass "row" $ mdo
      textInputValueD <- divClass "column column-67" $ do
        textInputValueD' <- (fmap . fmap) T.unpack $ divClassDyn isInvalidD $ textFieldAttrNoLabel (M.singleton "class" "mb-0") never never txtInit
        when isAndroid (availableBalanceWidget unitD)
        pure textInputValueD'
      unitD <- divClass "column column-33" $ do
        unitsDropdown (getUnitBTC unitInit) allUnitsBTC
      let dataDyn = zipDyn3 dustThresholdD unitD textInputValueD
      pure $ (\(dustThreshold, units, val) -> fmap (units,) $ toEither $ validateBtcAmount dustThreshold units val) <$> dataDyn
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
