{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Data.Maybe (fromMaybe)

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Filters.Btc
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Worker.Node

import Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Network.Wreq
import Network.Haskoin.Transaction
import Network.Haskoin.Address

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
  anon_name <- getWalletName
#ifdef ANDROID
  c <- liftIO $ loadCounter
  liftIO $ saveCounter $ PatternTries $ Map.insert anon_name 0 (patterntriesCount c)
#endif
  wrapper BalancesTitle (Just $ pure balancesPage) False $ divClass "balances-wrapper" $ do
    syncWidget =<< getSyncProgress
    currenciesList anon_name

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
        bal <- balancesGetting cur
        let setUs = getSettingsUnits settings
        divClass "currency-name"    $ text $ currencyName cur
        divClass "currency-balance" $ do
          elClass "span" "currency-value" $ dynText $ (\v -> showMoneyUnit v setUs) <$> bal
          elClass "span" "currency-unit"  $ dynText $ ffor bal $ \(Money c _) -> symbolUnit c setUs
          elClass "span" "currency-arrow" $ text "〉"
      pure $ cur <$ domEvent Click e
    getSettingsUnits = fromMaybe defUnits . settingsUnits

balancesGetting :: MonadFront t m => Currency -> m (Dynamic t Money)
balancesGetting cur = do
  ps <- getPubStorage
  pubSD <- getPubStorageD
  let allBtcAddrsD = ffor pubSD $ \(PubStorage _ cm _) -> case M.lookup BTC cm of
        Nothing -> []
        Just (CurrencyPubStorage keystore txmap) -> extractAddrs keystore

  abS <- sampleDyn allBtcAddrsD
  
  hD <- holdDyn (Money cur (calcSum (gbA abS) ps)) $ poke (updated pubSD) $ \pbs -> do
    allbtcAdrS <- sampleDyn allBtcAddrsD
    pure $ Money cur $ calcSum (gbA allbtcAdrS) pbs

  pure hD
  where
    calcSum ac pubS = case cur of
      BTC  -> calcBalance ac $ _currencyPubStorage'transactions <$> Map.lookup cur (_pubStorage'currencyPubStorages pubS)
      ERGO -> 0
    calcBalance ac mTxs = case mTxs of
      Nothing -> 0
      Just txs -> sum $ fmap (\(_,tx) -> case tx of
        BtcTx btx -> sum $ fmap (\(_,a) -> outValue a) $ L.filter (cselem ac) $ L.filter csbool $ fmap (\txo -> (getSegWitAddr txo,txo)) $ txOut btx
        ErgTx etx -> 0
        ) $ Map.toList txs
    csbool (a,_) = case a of
      Just b -> True
      Nothing -> False
    cselem ac (a,_) = case a of
      Just b -> (elem ((fromSegWit b)) ac)
      Nothing -> False
    gbA s = fmap (\(_,a) -> getBtcAddr a) s

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 0

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
