{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Data.Maybe (fromMaybe, fromJust)

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Page.Send
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Wrapper

import Control.Monad.IO.Class
import Data.Map.Strict as Map

-- These are for nodeTestWidget
import qualified Data.Dependent.Map as DM
import qualified Data.Text as T
import Ergvein.Wallet.Node
import Ergvein.Wallet.Native
import Network.Haskoin.Network
import Control.Monad.Random
import Ergvein.Wallet.Node.BTC
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Platform
import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Ergvein.Crypto.Hash
import Data.ByteString(ByteString)
import Control.Monad.Catch
import Ergvein.Wallet.Headers.Btc.Queries
import Ergvein.Wallet.Blocks.BTC
import Ergvein.Wallet.Blocks.BTC.Queries

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

nodeTestWidget :: MonadFront t m => m ()
nodeTestWidget = do
  conMapD <- getNodeConnectionsD
  void . widgetHoldDyn $ ffor conMapD $ \cm -> do
    let node   = head $ Map.elems $ fromJust $ DM.lookup BTCTag cm
    let respE  = nodeconRespE node
    let closeE = nodeconCloseE node
    let fire   = nodeconReqFire node
    logShowInfoMsg $ ("closeE" :: Text) <$ closeE
    goE <- outlineButton ("Go" :: Text)
    let bh :: BlockHash = "000000000000000000011e66c8f568558750ce1396630e1fa49d3ecd440b1e9b"
    mblockE <- requestBTCBlockNode node $ bh <$ never
    performEvent_ $ (logWrite . showt . maybe Nothing (Just . blockHeader)) <$> mblockE
    insE <- performEvent $ fforMaybe mblockE $ \case
      Nothing -> Nothing
      Just blk -> Just $ insertBTCBlock blk
    mblkE <- performEvent $ ffor goE $ const $ getBTCBlock bh
    performEvent $ (logWrite . showt . maybe Nothing (Just . blockHeader)) <$> mblkE

storeTestWidget :: MonadFront t m => m ()
storeTestWidget = do
  goE <- outlineButton ("Rand" :: Text)
  let bh :: BlockHash = "000000000000000000011e66c8f568558750ce1396630e1fa49d3ecd440b1e9b"
  mblockE <- requestBTCBlockRandNode $ bh <$ goE
  performEvent_ $ (logWrite . showt . maybe Nothing (Just . blockHeader)) <$> mblockE

  goMulE <- outlineButton ("Go mul" :: Text)
  let bh' :: BlockHash = "0000000000000000000dfd9382d5fa9d39b635745817e83e4da9366937067d4c"

  -- blkRespE <- requestBTCBlockConfirm 2 . (<$) bh' =<< getPostBuild
  -- blkE <- performEvent $ ffor blkRespE $ \case
  --   BRMNotFound   -> logWrite "BRMNotFound" >> pure Nothing
  --   BRMConflict   -> logWrite "BRMConflict" >> pure Nothing
  --   BRMBlock blk  -> (logWrite $ showt $ blockHeader blk) >> pure (Just blk)
  mblkE <- getBlockByE . (<$) bh' =<< getPostBuild -- storeBlockByE (fmapMaybe id blkE)
  performEvent $ (logWrite . showt . maybe Nothing (Just . blockHeader)) <$> mblkE
  pure ()

currenciesList :: MonadFront t m => Text -> m ()
currenciesList name = divClass "currency-content" $ do
  nodeTestWidget
  storeTestWidget
  s <- getSettings
  historyE <- leftmost <$> traverse (currencyLine s) (getActiveCurrencies s)
  let thisWidget = Just $ pure balancesPage
  void $ nextWidget $ ffor historyE $ \cur -> Retractable {
    retractableNext = historyPage cur
  , retractablePrev = thisWidget
  }
  where
    currencyLine settings cur = do
      (e, _) <- divClass' "currency-row" $ do
        bal <- currencyBalance cur
        let setUs = getSettingsUnits settings
        langD <- getLanguage
        divClass "currency-name"    $ text $ currencyName cur
        divClass "currency-balance" $ do
          elClass "span" "currency-value" $ dynText $ (\v -> showMoneyUnit v setUs) <$> bal
          elClass "span" "currency-unit"  $ dynText $ ffor2 bal langD $ \(Money cur _) lang -> symbolUnit cur lang setUs
          elClass "span" "currency-arrow" $ text "〉"
      pure $ cur <$ domEvent Click e
    getActiveCurrencies s = fromMaybe allCurrencies $ Map.lookup name $ activeCurrenciesMap $ settingsActiveCurrencies s
    getSettingsUnits = fromMaybe defUnits . settingsUnits

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 1

symbolUnit :: Currency -> Language -> Units -> Text
symbolUnit cur lang units =
  case cur of
    BTC  -> case getUnitBTC units of
              BtcWhole    -> "btc"
              BtcMilli    -> "mbtc"
              BtcSat      -> "sat"
    ERGO -> case getUnitERGO units of
              ErgWhole    -> "erg"
              ErgMilli    -> "merg"
              ErgNano     -> "nerg"
