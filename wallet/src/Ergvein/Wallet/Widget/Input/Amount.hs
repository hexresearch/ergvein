{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Widget.Input.Amount(
      sendAmountWidgetBtc
  ) where

import Control.Monad.Except
import Data.Maybe
import Data.Word

import Ergvein.Either
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Validate
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

-- | Input field with units. Converts everything to satoshis and returns the unit.
sendAmountWidgetBtc :: (MonadFront t m) => Maybe (UnitBTC, Word64) -> Event t () -> m (Dynamic t (Maybe (UnitBTC, Word64)))
sendAmountWidgetBtc minit submitE = divClass "amount-input" $ mdo
  let errsD = fromMaybe [] <$> amountErrsD
      isInvalidD = fmap (maybe "" (const "is-invalid")) amountErrsD
  amountValD <- do
    el "label" $ localizedText AmountString
    divClass "row" $ mdo
      textInputValueD <- divClass "column column-67" $ do
        textInputValueD' <- do
          txtInit <- do
            units <- getSettingsUnitBtc
            let unitInit = maybe units fst minit
            pure $ maybe "" (\(_, amount) -> showMoneyUnit (Money BTC amount) unitInit) minit
          divClassDyn isInvalidD $ textFieldAttrNoLabel (M.singleton "class" "mb-0") never never txtInit
        when isAndroid (availableBalanceWidget BTC unitD)
        pure textInputValueD'
      unitD <- divClass "column column-33" $ do
          units <- getSettingsUnitBtc
          let unitInit = maybe units fst minit
          unitsDropdown unitInit allUnitsBTC
      pure $ zipDynWith (\u v -> fmap (u,) $ toEither $ validateAmount 0 u v) unitD textInputValueD
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  amountErrsD <- holdDyn Nothing $ ffor (current amountValD `tag` submitE) eitherToMaybe'
  pure $ eitherToMaybe <$> amountValD
