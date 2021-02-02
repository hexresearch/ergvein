-- {-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Widget.Input.BTC.Amount(
    sendAmountWidget
  ) where

import Control.Lens
import Control.Monad.Except
import Data.Maybe
import Data.Word
import Text.Read

import Ergvein.Text
import Ergvein.Types
import Ergvein.Types.Derive
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Elements.Input
import Ergvein.Wallet.Elements.Toggle
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Fee
import Ergvein.Wallet.Localization.Send
import Ergvein.Wallet.Localization.Settings()
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Node
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Transaction.Builder
import Ergvein.Wallet.Transaction.Util
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Wrapper

import Network.Haskoin.Network (Inv(..), InvVector(..), InvType(..), Message(..))

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Haskoin.Address as HA
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Transaction as HT

-- | Input field with units. Converts everything to satoshis and returns the unit
sendAmountWidget :: MonadFront t m => Maybe (UnitBTC, Word64) -> Event t () -> m (Dynamic t (Maybe (UnitBTC, Word64)))
sendAmountWidget minit validateE = mdo
  setUs <- fmap (fromMaybe defUnits . settingsUnits) getSettings
  let (unitInit, txtInit) = maybe (setUs, "") (\(u, a) -> let us = Units (Just u) Nothing
        in (us, showMoneyUnit (Money BTC a) us)) minit
  let errsD = fmap (maybe [] id) amountErrsD
  let isInvalidD = fmap (maybe "" (const "is-invalid")) amountErrsD
  amountValD <- el "div" $ mdo
    textInputValueD <- (fmap . fmap) T.unpack $ divClassDyn isInvalidD $ textField AmountString txtInit
    when isAndroid (availableBalanceWidget unitD)
    unitD <- unitsDropdown (getUnitBTC unitInit) allUnitsBTC
    pure $ zipDynWith (\u v -> fmap (u,) $ toEither $ validateBtcWithUnits u v) unitD textInputValueD
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  amountErrsD <- holdDyn Nothing $ ffor (current amountValD `tag` validateE) (either Just (const Nothing))
  pure $ (either (const Nothing) Just) <$> amountValD
  where
    availableBalanceWidget uD = do
      balanceValue <- balancesWidget BTC
      balanceText <- localized SendAvailableBalance
      let balanceVal = zipDynWith (\x y -> showMoneyUnit x (Units (Just y) Nothing) <> " " <> btcSymbolUnit y) balanceValue uD
          balanceTxt = zipDynWith (\x y -> x <> ": " <> y) balanceText balanceVal
      divClass "send-page-available-balance" $ dynText balanceTxt
    unitsDropdown val allUnits = do
      langD <- getLanguage
      let unitD = constDyn val
      initKey <- sample . current $ unitD
      let listUnitsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allUnits
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated unitD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
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
