module Ergvein.Wallet.Widget.Balance(
    balancesWidget
  , balanceTitleWidget
  , balanceTitleWidgetSimple
  ) where

import Control.Lens
import Data.Maybe (fromMaybe)

import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Utxo
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.History
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings

import qualified Data.List as L
import qualified Data.Map.Strict as M

ergoBalances :: MonadFront t m => m (Dynamic t Money)
ergoBalances = pure $ pure $ Money ERGO 0

btcBalances :: MonadFront t m => m (Dynamic t Money)
btcBalances = do
  pubStorageD <- getPubStorageD
  pure $ ffor pubStorageD $ \pubStorage -> let
    utxos = M.elems $ fromMaybe M.empty $ pubStorage ^. pubStorage'currencyPubStorages . at BTC & fmap (view currencyPubStorage'utxos)
    in Money BTC $ L.foldl' helper 0 utxos
  where
    helper :: MoneyUnit -> UtxoMeta -> MoneyUnit
    helper balance UtxoMeta{utxoMeta'status = EUtxoSending _} = balance
    helper balance UtxoMeta{..} = balance + utxoMeta'amount

balancesWidget :: MonadFront t m => Currency -> m (Dynamic t Money)
balancesWidget cur = case cur of
  ERGO -> ergoBalances
  BTC  -> btcBalances

balanceTitleWidget :: MonadFront t m => Currency -> m (Dynamic t Text)
balanceTitleWidget cur = do
  bal <- balancesWidget cur
  settings <- getSettings
  titleText <- localized $ HistoryBalance
  let getSettingsUnits = fromMaybe defUnits . _settingsUnits
      setUs = getSettingsUnits settings
      titleVal = ffor bal (\v -> showMoneyUnit v setUs)
      curSymbol = symbolUnit cur setUs
      title = zipDynWith (\x y -> x <> ": " <> y <> " " <> curSymbol) titleText titleVal
  pure title

balanceTitleWidgetSimple :: MonadFront t m => Currency -> m (Dynamic t Text)
balanceTitleWidgetSimple cur = do
  bal <- balancesWidget cur
  settings <- getSettings
  let getSettingsUnits = fromMaybe defUnits . _settingsUnits
      setUs = getSettingsUnits settings
      titleVal = ffor bal (\v -> showMoneyUnit v setUs)
      curSymbol = symbolUnit cur setUs
      title = (\x -> x <> " " <> curSymbol) <$> titleVal
  pure title