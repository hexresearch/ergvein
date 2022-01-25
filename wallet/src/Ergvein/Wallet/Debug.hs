{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Ergvein.Wallet.Debug
  (
    debugPage
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Serialize
import Data.Traversable (for)
import Network.Haskoin.Transaction

import Ergvein.Text
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Localize ()
import Ergvein.Wallet.Localize.Settings
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Network.Haskoin.Keys as HK

import Network.Socket
import qualified Control.Exception.Safe as Ex

data DebugBtns
  = DbgUtxo
  | DbgPubInt
  | DbgPubExt
  | DbgPrvInt
  | DbgPrvExt
  | DbgMnemonic
  | DbgTxs
  | DbgReplacedTxs
  | DbgPossiblyReplacedTxs

mkTxt :: Text -> Text
mkTxt = id

backTxt :: Text
backTxt = "Back"

debugPage :: MonadFront t m => m ()
debugPage = do
  title <- localized STPSButDebug
  let thisWidget = Just $ pure debugPage
  wrapper False title thisWidget $ mdo
    h5 . dynText . ("Current height: " <>) . fmap showt =<< getCurrentHeight BTC
    h5 . dynText . ("Scanned height: " <>) . fmap showt =<< getScannedHeightD BTC
    avgD <- indexersAverageLatencyWidget =<< delay 1 pingE
    h5 . dynText $ do
      p <- avgD
      pure $ "Avg.indexer ping: " <> showt p
    pingE <- outlineButton $ mkTxt "Ping"
    divClass "grid1 mx-a" $ do
      utxoE <- (DbgUtxo <$) <$> outlineButton ("UTXO" :: Text)
      pubIntE <- fmap (DbgPubInt <$) $ outlineButton $ mkTxt "Pub Internals"
      pubExtE <- fmap (DbgPubExt <$) $ outlineButton $ mkTxt "Pub Externals"
      prvIntE <- fmap (DbgPrvInt <$) $ outlineButton $ mkTxt "Priv Internals"
      prvExtE <- fmap (DbgPrvExt <$) $ outlineButton $ mkTxt "Priv Externals"
      txsE <- fmap (DbgTxs <$) $ outlineButton $ mkTxt "Txs"
      replacedTxsE <- fmap (DbgReplacedTxs <$) $ outlineButton $ mkTxt "Replaced txs"
      possiblyReplacedTxsE <- fmap (DbgPossiblyReplacedTxs <$) $ outlineButton $ mkTxt "Possibly replaced txs"
      mnemonicE <- (DbgMnemonic <$) <$> outlineButton ("Mnemonic" :: Text)
      let goE = leftmost [utxoE, pubIntE, pubExtE, prvIntE, prvExtE, txsE, replacedTxsE, possiblyReplacedTxsE, mnemonicE]
      void $ nextWidget $ ffor goE $ \sel -> Retractable {
          retractableNext = case sel of
            DbgUtxo                -> dbgUtxoPage
            DbgPubInt              -> dbgPubInternalsPage
            DbgPubExt              -> dbgPubExternalsPage
            DbgPrvInt              -> dbgPrivInternalsPage
            DbgPrvExt              -> dbgPrivExternalsPage
            DbgMnemonic            -> dbgMnemonicPage
            DbgTxs                 -> dbgTxsPage
            DbgReplacedTxs         -> dbgReplacedTxsPage
            DbgPossiblyReplacedTxs -> dbgPossiblyReplacedTxsPage
        , retractablePrev = thisWidget
        }

addUrlWidget :: forall t m . MonadFront t m => Dynamic t Bool -> m (Event t SockAddr)
addUrlWidget showD = fmap switchDyn $ networkHoldDyn $ ffor showD $ \b -> if not b then pure never else do
  murlE <- el "div" $ do
    textD <- fmap _inputElement_value $ inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "text")
    goE <- outlineButton $ mkTxt "Addurl"
    performFork $ ffor goE $ const $ do
      t <- sampleDyn textD
      let (h,p) = T.drop 1 <$> T.span (/= ':') t
      let hints = defaultHints { addrFlags = [AI_ALL] , addrSocketType = Stream }
      addrs <- liftIO $ Ex.catch (
          getAddrInfo (Just hints) (Just $ T.unpack h) (Just $ T.unpack p)
        ) (\(_ :: Ex.SomeException) -> pure [])
      pure $ addrAddress <$> listToMaybe addrs
  void $ networkHold (pure ()) $ ffor murlE $ \case
    Nothing -> divClass "form-field-errors" $ text "Falied to parse URL"
    _ -> pure ()
  pure $ fmapMaybe id murlE

dbgUtxoPage :: MonadFront t m => m ()
dbgUtxoPage = wrapper False "UTXO" (Just $ pure dbgUtxoPage) $ divClass "currency-content" $ do
  pubSD <- getPubStorageD
  let utxoD = ffor pubSD $ \ps -> M.toList $ fromMaybe M.empty $ ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'utxos
  void $ networkHoldDyn $ ffor utxoD $ \utxo -> divClass "" $ do
    for_ utxo $ \(o, BtcUtxoMeta{..}) -> do
      divClass "word-break-all" $ text $ showt $ outPointHash o
      el "div" $ text $ showt (btcUtxo'purpose, btcUtxo'index) <> " amount: " <> showt btcUtxo'amount <> " " <> showt btcUtxo'status
      divClass "word-break-all" $ text $ showt btcUtxo'script
      el "div" $ text "------------------------------------------"

dbgPubInternalsPage :: MonadFront t m => m ()
dbgPubInternalsPage = wrapper False "Internal public  keys" (Just $ pure dbgPubInternalsPage) $ divClass "currency-content" $ do
  pubSD <- getPubStorageD
  let intsD = ffor pubSD $ \ps -> V.indexed $ maybe V.empty (\a -> a ^. currencyPubStorage'pubKeystore & pubKeystore'internal) (ps ^. pubStorage'currencyPubStorages . at BTC)
  void $ networkHoldDyn $ ffor intsD $ \ints -> divClass "" $ do
    for ints $ \(i, EgvPubKeyBox{..}) -> do
      let keyTxt = egvAddrToText $ egvXPubKeyToEgvAddress pubKeyBox'key
      divClass "word-break-all" $ text $ showt i <> ": " <> keyTxt <> "; Txs: " <> showt (S.size pubKeyBox'txs) <> " Man: " <> showt pubKeyBox'manual

dbgPubExternalsPage :: MonadFront t m => m ()
dbgPubExternalsPage = wrapper False "External public keys" (Just $ pure dbgPubExternalsPage) $ divClass "currency-content" $ do
  pubSD <- getPubStorageD
  let intsD = ffor pubSD $ \ps -> V.indexed $ maybe V.empty (\a -> a ^. currencyPubStorage'pubKeystore & pubKeystore'external) (ps ^. pubStorage'currencyPubStorages . at BTC)
  void $ networkHoldDyn $ ffor intsD $ \ints -> divClass "" $ do
    for ints $ \(i, EgvPubKeyBox{..}) -> do
      let keyTxt = egvAddrToText $ egvXPubKeyToEgvAddress pubKeyBox'key
      divClass "word-break-all" $ text $ showt i <> ": " <> keyTxt <> "; Txs: " <> showt (S.size pubKeyBox'txs) <> " Man: " <> showt pubKeyBox'manual

dbgPrivInternalsPage :: MonadFront t m => m ()
dbgPrivInternalsPage = wrapper False "Private internal keys" (Just $ pure dbgPrivInternalsPage) $ divClass "currency-content" $ do
  buildE <- getPostBuild
  intE <- withWallet $ ffor buildE $ \_ prv -> do
    let PrvKeystore _ _ int = prv ^. prvStorage'currencyPrvStorages
          . at BTC . non (error "btcSendConfirmationWidget: not exsisting store!")
          . currencyPrvStorage'prvKeystore
    pure $ V.indexed int
  void $ networkHold (pure ()) $ ffor intE $ \ints -> divClass "" $ do
    for_ ints $ \(i, k) -> do
      let k' = unEgvXPrvKey k
      let p = egvAddrToText $ egvXPubKeyToEgvAddress $ flip BtcXPubKey "" $ HK.deriveXPubKey k'
      el "div" $ text $ showt i <> " ------------------------------------------"
      divClass "word-break-all" $ text $ showt k'
      divClass "word-break-all" $ text p

dbgPrivExternalsPage :: MonadFront t m => m ()
dbgPrivExternalsPage = wrapper False "Private external keys" (Just $ pure dbgPrivExternalsPage) $ divClass "currency-content" $ do
  buildE <- getPostBuild
  extE <- withWallet $ ffor buildE $ \_ prv -> do
    let PrvKeystore _ ext _ = prv ^. prvStorage'currencyPrvStorages
          . at BTC . non (error "btcSendConfirmationWidget: not exsisting store!")
          . currencyPrvStorage'prvKeystore
    pure $ V.indexed ext
  void $ networkHold (pure ()) $ ffor extE $ \exts -> divClass "" $ do
    for_ exts $ \(i, k) -> do
      let k' = unEgvXPrvKey k
      let p = egvAddrToText $ egvXPubKeyToEgvAddress $ flip BtcXPubKey "" $ HK.deriveXPubKey k'
      el "div" $ text $ showt i <> " ------------------------------------------"
      divClass "word-break-all" $ text $ showt k'
      divClass "word-break-all" $ text p

dbgMnemonicPage :: MonadFront t m => m ()
dbgMnemonicPage = wrapper False "Mnemonic" (Just $ pure dbgMnemonicPage) $ divClass "currency-content" $ do
  buildE <- getPostBuild
  mnemonicE <- withWallet $ ffor buildE $ \_ prv -> do
    pure $ prv ^. prvStorage'mnemonic
  mnemonicD <- holdDyn "" mnemonicE
  dynText mnemonicD

dbgTxsPage :: MonadFront t m => m ()
dbgTxsPage = wrapper False "Transactions" (Just $ pure dbgTxsPage) $ divClass "currency-content" $ do
  psD <- getPubStorageD
  void $ networkHoldDyn $ ffor psD $ \ps -> do
    let txs = ps ^. btcPubStorage . currencyPubStorage'transactions
    for_ txs $ \case
      TxBtc (BtcTx tx _) -> do
        divClass "word-break-all" $ el "b" $ text $ showt $ txHash tx
        divClass "word-break-all" $ text $ showt tx
        divClass "word-break-all" $ text $ bs2Hex $ encode tx
        el "div" $ text "------------------------------------------------------"

dbgReplacedTxsPage :: MonadFront t m => m ()
dbgReplacedTxsPage = wrapper False "Replaced transactions" (Just $ pure dbgReplacedTxsPage) $ divClass "currency-content" $ do
  psD <- getPubStorageD
  void $ networkHoldDyn $ ffor psD $ \ps -> do
    let txMap = ps ^. btcPubStorage . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'replacedTxs
    for_ (M.toList txMap) $ \(txId, replacedTxs) -> do
      divClass "word-break-all" $ text $ showt txId <> ": " <> showt replacedTxs

dbgPossiblyReplacedTxsPage :: MonadFront t m => m ()
dbgPossiblyReplacedTxsPage = wrapper False "Possibly replaced transactions" (Just $ pure dbgPossiblyReplacedTxsPage) $ divClass "currency-content" $ do
  psD <- getPubStorageD
  void $ networkHoldDyn $ ffor psD $ \ps -> do
    let txMap = ps ^. btcPubStorage . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'possiblyReplacedTxs
    for_ (M.toList txMap) $ \(txId, possiblyReplacedTxs) -> do
      divClass "word-break-all" $ text $ showt txId <> ": " <> showt possiblyReplacedTxs
