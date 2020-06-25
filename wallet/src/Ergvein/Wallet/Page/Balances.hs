{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Data.Maybe (fromMaybe, isJust)

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
import qualified Data.List as L
import Network.Wreq
import Network.Haskoin.Transaction
import Network.Haskoin.Address

#ifdef ANDROID
import Control.Monad.IO.Class
#endif

-- Debug imports
import Control.Lens
import Data.Maybe
import Ergvein.Types.AuthInfo
import Ergvein.Types.Utxo
import Ergvein.Wallet.Native

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

testPage :: MonadFront t m => m ()
testPage = do
  aiD <- getAuthInfo
  -- el "div" $ text "AAAAAAAAAA"
  let op = OutPoint {outPointHash = "a0819d01cb5de6586a2b6ecdcee2a83a5f59f1f73a1152478aa117eff2765bfe", outPointIndex = 0}
  let aaaa = (fromList [(op,(10000,EUtxoReceiving))],[])
  let del = (Map.empty, [op])
  addE <- fmap (aaaa <$) $ outlineButton ("addE" :: Text)
  goE <- fmap ((Map.empty, [op]) <$) $ outlineButton ("goE" :: Text)
  -- performFork_ $ (logWrite . showt) <$> goE
  updateBtcUtxoSet $ leftmost [addE, goE]
  widgetHoldDyn $ ffor aiD $ \ai -> do
    let pts = ai ^.
            authInfo'storage
          . storage'pubStorage
          . pubStorage'currencyPubStorages
          . at BTC
          & fmap (getBtcUtxoSetFromStore . _currencyPrvStorage'utxos)
          & Map.toList . fromMaybe Map.empty . join
    flip traverse pts $ \(OutPoint h i, (v,s)) -> el "div" $ do
      el "div" $ text $ "h: " <> showt h <> " i: " <> showt i
      el "div" $ text $ "v: " <> showt v <> " s: " <> showt s
      el "div" $ text "---------------------------------------------------"
    pure ()
  pure ()

balancesPage :: MonadFront t m => m ()
balancesPage = do
  anon_name <- getWalletName
  testPage
#ifdef ANDROID
  c <- liftIO $ loadCounter
  liftIO $ saveCounter $ PatternTries $ Map.insert anon_name 0 (patterntriesCount c)
#endif
  wrapper False BalancesTitle (Just $ pure balancesPage) $ do
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
  let allBtcAddrsD = ffor pubSD $ \(PubStorage _ cm _ _) -> case Map.lookup BTC cm of
        Nothing -> []
        Just (CurrencyPubStorage keystore txmap _ _ _) -> extractAddrs keystore
  abS <- sampleDyn allBtcAddrsD
  hD <- holdDyn (Money cur (calcSum (fmap (getBtcAddr . snd) abS) ps)) $ poke (updated pubSD) $ \pbs -> do
    allbtcAdrS <- sampleDyn allBtcAddrsD
    pure $ Money cur $ calcSum (fmap (getBtcAddr . snd) allbtcAdrS) pbs

  pure hD
  where
    calcSum ac pubS = case cur of
      BTC  -> calcBalance ac $ _currencyPubStorage'transactions <$> Map.lookup cur (_pubStorage'currencyPubStorages pubS)
      ERGO -> 0
    calcBalance ac mTxs = case mTxs of
      Nothing -> 0
      Just txs -> sum $ fmap (\(_,tx) -> case tx of
        BtcTx btx -> sum $ fmap (outValue . snd) $ L.filter (maybe False (flip elem ac . fromSegWit) . fst) $ fmap (\txo -> (getSegWitAddr txo,txo)) $ txOut btx
        ErgTx etx -> 0
        ) $ Map.toList txs

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
