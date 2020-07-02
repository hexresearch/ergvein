{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Control.Lens
import Data.Maybe (fromMaybe, isJust)
import Data.Word
import Network.Haskoin.Address
import Network.Haskoin.Transaction
import Network.Wreq

import Ergvein.Filters.Btc
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Worker.Node
import Ergvein.Wallet.Wrapper

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

#ifdef ANDROID
import Control.Monad.IO.Class
#endif

data BalancesStrings
  = BalancesTitle
  | ButtonSend
  | ButtonReceive

instance LocalizedPrint BalancesStrings where
  localizedShow l v = case l of
    English -> case v of
      BalancesTitle  -> "Default wallet"
      ButtonSend ->  "Send"
      ButtonReceive -> "Receive"
    Russian -> case v of
      BalancesTitle  -> "Стандартный кошелек"
      ButtonSend ->  "Отправить"
      ButtonReceive -> "Получить"

balancesPage :: MonadFront t m => m ()
balancesPage = do
  walletName <- getWalletName
#ifdef ANDROID
  c <- liftIO $ loadCounter
  liftIO $ saveCounter $ PatternTries $ M.insert walletName 0 (patterntriesCount c)
#endif
  wrapper False walletName (Just $ pure balancesPage) $ do
    syncWidget =<< getSyncProgress
    currenciesList walletName

currenciesList :: MonadFront t m => Text -> m ()
currenciesList name = divClass "currency-content" $ do
  s <- getSettings
  ps <- getPubStorage
  pubSD <- getPubStorageD
  historyE <- leftmost <$> traverse (currencyLine s) (_pubStorage'activeCurrencies ps)
  if (settingsPortfolio s)
    then portfolioWidget
    else pure ()
  let thisWidget = Just $ pure balancesPage
  void $ nextWidget $ ffor historyE $ \cur -> Retractable {
    retractableNext = historyPage cur
  , retractablePrev = thisWidget
  }
  where
    currencyLine settings cur = do
      (e, _) <- divClass' "currency-row" $ do
        bal <- balancesWidget cur
        let setUs = getSettingsUnits settings
        divClass "currency-name"    $ text $ currencyName cur
        divClass "currency-balance" $ do
          elClass "span" "currency-value" $ dynText $ (\v -> showMoneyUnit v setUs) <$> bal
          elClass "span" "currency-unit"  $ text $ symbolUnit cur setUs
          elClass "span" "currency-arrow" $ text "〉"
      pure $ cur <$ domEvent Click e
    getSettingsUnits = fromMaybe defUnits . settingsUnits

balancesWidget :: MonadFront t m => Currency -> m (Dynamic t Money)
balancesWidget cur = case cur of
  ERGO -> balancesErgo
  BTC  -> btcBalances

balancesErgo :: MonadFront t m => m (Dynamic t Money)
balancesErgo = pure $ pure $ Money ERGO 0

btcBalances :: MonadFront t m => m (Dynamic t Money)
btcBalances = do
  pubSD <- getPubStorageD
  pure $ ffor pubSD $ \ps -> let
    utxo = M.elems $ fromMaybe M.empty $ join $ ps ^. pubStorage'currencyPubStorages . at BTC
      & \mcps -> ffor mcps $ \cps -> cps ^. currencyPubStorage'utxos & getBtcUtxoSetFromStore
    in Money BTC $ foo 0 utxo $ \s (v,_) -> s + v
  where foo b ta f = L.foldl' f b ta

symbolUnit :: Currency -> Units -> Text
symbolUnit cur units =
  case cur of
    BTC  -> case getUnitBTC units of
              BtcWhole    -> "btc"
              BtcMilli    -> "mbtc"
              BtcSat      -> "sat"
    ERGO -> case getUnitERGO units of
              ErgWhole    -> "erg"
              ErgMilli    -> "merg"
              ErgNano     -> "nerg"
