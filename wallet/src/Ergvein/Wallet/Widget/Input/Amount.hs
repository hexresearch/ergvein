{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Widget.Input.Amount(
      sendAmountWidgetBtc
  ) where

import Control.Monad.Except
import Data.Word

import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Orphanage ()
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Widget.Balance
import Sepulcas.Elements
import Sepulcas.Text (Display(..))

import qualified Data.Map.Strict as M

availableBalanceWidget :: (MonadFront t m, IsMoneyUnit a, Display a) => Currency -> Dynamic t a -> m ()
availableBalanceWidget cur uD = do
  balanceValue <- balanceWidget cur
  balanceTitle <- localized SendAvailableBalance
  let balanceVal = zipDynWith (\x y -> showMoneyUnit x y <> " " <> display y) balanceValue uD
      balanceText = zipDynWith (\x y -> x <> ": " <> y) balanceTitle balanceVal
  divClass "amount-available-balance" $ dynText balanceText

unitsDropdown :: (MonadFront t m, Ord a, LocalizedPrint a) => a -> [a] -> m (Dynamic t a)
unitsDropdown initialUnit allUnits = do
  langD <- getLanguage
  let listUnitsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allUnits
      config = DropdownConfig {
            _dropdownConfig_setValue = never
          , _dropdownConfig_attributes = constDyn ("class" =: "mb-0")
        }
  dp <- dropdown initialUnit listUnitsD config
  let selD = _dropdown_value dp
  holdUniqDyn selD

-- | Amount input field with units.
sendAmountWidgetBtc :: (MonadFront t m, LocalizedPrint l)
  => Maybe (Word64, UnitBTC)    -- ^ Inital unit and value
  -> Event t Text               -- ^ Event that updates input value
  -> Dynamic t [l] -- ^ Dynamic with errors
  -> m (Dynamic t Text, Dynamic t UnitBTC, Event t ())
sendAmountWidgetBtc minit setValE errsD = divClass "amount-input" $ do
  units <- getSettingsUnitBtc
  let unitInit = maybe units snd minit
      initAmountTxt = maybe "" (\(amount, _) -> showMoneyUnit (Money BTC amount) unitInit) minit
      inputConfig = def
        & textInputConfig_initialValue .~ initAmountTxt
        & textInputConfig_setValue .~ setValE
  (amountD, btnEvents, unitD) <- labeledTextFieldWithBtnsAndSelector
    AmountString
    inputConfig
    [localizedText SendAll]
    (unitsDropdown unitInit allUnitsBTC)
    errsD
  let sendAllBtnE = head btnEvents
  when isAndroid (availableBalanceWidget BTC unitD)
  pure (amountD, unitD, sendAllBtnE)
