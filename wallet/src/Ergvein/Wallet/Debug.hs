{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Ergvein.Wallet.Debug
  (
    debugWidget
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Serialize
import Network.Haskoin.Transaction
import Data.String (fromString)

import Ergvein.Core.Node.Btc.Blocks
import Ergvein.Text
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Localize ()
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Network.Haskoin.Keys as HK
import qualified Network.Haskoin.Block as HB

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
  | DbgBlocks

mkTxt :: Text -> Text
mkTxt = id

backTxt :: Text
backTxt = "Back"

debugWidget :: MonadFront t m => m ()
debugWidget = el "div" $ do
  utxoE <- fmap (DbgUtxo <$) $ outlineButton ("UTXO" :: Text)
  pubIntE <- fmap (DbgPubInt <$) $ outlineButton $ mkTxt "Pub Internals"
  pubExtE <- fmap (DbgPubExt <$) $ outlineButton $ mkTxt "Pub Externals"
  prvIntE <- fmap (DbgPrvInt <$) $ outlineButton $ mkTxt "Priv Internals"
  prvExtE <- fmap (DbgPrvExt <$) $ outlineButton $ mkTxt "Priv Externals"
  blksE   <- fmap (DbgBlocks <$) $ outlineButton $ mkTxt "Debug blocks"
  txsE <- fmap (DbgTxs <$) $ outlineButton $ mkTxt "Txs"
  mnemonicE <- fmap (DbgMnemonic <$) $ outlineButton ("Mnemonic" :: Text)
  pingE <- outlineButton $ mkTxt "Ping"
  avgD <- indexersAverageLatencyWidget =<< delay 1 pingE
  h5 . dynText $ do
    p <- avgD
    pure $ "Avg.indexer ping: " <> showt p
  h5 . dynText . (\s -> "Current height: " <> s) . fmap showt =<< getCurrentHeight BTC
  h5 . dynText . (\s -> "Scanned height: " <> s) . fmap showt =<< getScannedHeightD BTC

  let goE = leftmost [utxoE, pubIntE, pubExtE, prvIntE, prvExtE, txsE, mnemonicE, blksE]
  void $ nextWidget $ ffor goE $ \sel -> Retractable {
      retractableNext = case sel of
        DbgUtxo     -> dbgUtxoPage
        DbgPubInt   -> dbgPubInternalsPage
        DbgPubExt   -> dbgPubExternalsPage
        DbgPrvInt   -> dbgPrivInternalsPage
        DbgPrvExt   -> dbgPrivExternalsPage
        DbgMnemonic -> dbgMnemonicPage
        DbgTxs      -> dbgTxsPage
        DbgBlocks   -> dbgBlocksPage
    , retractablePrev = Nothing
    }

addUrlWidget :: forall t m . MonadFront t m => Dynamic t Bool -> m (Event t SockAddr)
addUrlWidget showD = fmap switchDyn $ networkHoldDyn $ ffor showD $ \b -> if not b then pure never else do
  murlE <- el "div" $ do
    textD <- fmap _inputElement_value $ inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "text")
    goE <- outlineButton $ mkTxt "Addurl"
    performFork $ ffor goE $ const $ do
      t <- sampleDyn textD
      let (h,p) = fmap (T.drop 1) $ T.span (/= ':') t
      let hints = defaultHints { addrFlags = [AI_ALL] , addrSocketType = Stream }
      addrs <- liftIO $ Ex.catch (
          getAddrInfo (Just hints) (Just $ T.unpack h) (Just $ T.unpack p)
        ) (\(_ :: Ex.SomeException) -> pure [])
      pure $ fmap addrAddress $ listToMaybe addrs
  void $ networkHold (pure ()) $ ffor murlE $ \case
    Nothing -> divClass "form-field-errors" $ text "Falied to parse URL"
    _ -> pure ()
  pure $ fmapMaybe id murlE

dbgUtxoPage :: MonadFront t m => m ()
dbgUtxoPage = wrapper False "UTXO" (Just $ pure dbgUtxoPage) $ divClass "currency-content" $ do
  void . el "div" $ retract =<< outlineButton backTxt
  pubSD <- getPubStorageD
  let utxoD = ffor pubSD $ \ps -> M.toList $ fromMaybe M.empty $ ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'utxos
  void $ networkHoldDyn $ ffor utxoD $ \utxo -> divClass "" $ do
    void $ flip traverse utxo $ \(o, BtcUtxoMeta{..}) -> do
      el "div" $ text $ showt $ outPointHash o
      el "div" $ text $ showt (btcUtxo'purpose, btcUtxo'index) <> " amount: " <> showt btcUtxo'amount <> " " <> showt btcUtxo'status
      el "div" $ text $ showt btcUtxo'script
      el "div" $ text $ "------------------------------------------"

dbgPubInternalsPage :: MonadFront t m => m ()
dbgPubInternalsPage = wrapper False "Public internal keys" (Just $ pure dbgPubInternalsPage) $ divClass "currency-content" $ do
  void . el "div" $ retract =<< outlineButton backTxt
  pubSD <- getPubStorageD
  let intsD = ffor pubSD $ \ps -> V.indexed $ fromMaybe V.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (\a -> a ^. currencyPubStorage'pubKeystore & pubKeystore'internal)
  void $ networkHoldDyn $ ffor intsD $ \ints -> divClass "" $ do
    flip traverse ints $ \(i, EgvPubKeyBox{..}) -> do
      let keyTxt = egvAddrToText $ egvXPubKeyToEgvAddress pubKeyBox'key
      el "div" $ text $ showt i <> ": " <> keyTxt <> "; Txs: " <> showt (S.size pubKeyBox'txs) <> " Man: " <> showt pubKeyBox'manual

dbgPubExternalsPage :: MonadFront t m => m ()
dbgPubExternalsPage = wrapper False "Public external keys" (Just $ pure dbgPubExternalsPage) $ divClass "currency-content" $ do
  void . el "div" $ retract =<< outlineButton backTxt
  pubSD <- getPubStorageD
  let intsD = ffor pubSD $ \ps -> V.indexed $ fromMaybe V.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (\a -> a ^. currencyPubStorage'pubKeystore & pubKeystore'external)
  void $ networkHoldDyn $ ffor intsD $ \ints -> divClass "" $ do
    flip traverse ints $ \(i, EgvPubKeyBox{..}) -> do
      let keyTxt = egvAddrToText $ egvXPubKeyToEgvAddress pubKeyBox'key
      el "div" $ text $ showt i <> ": " <> keyTxt <> "; Txs: " <> showt (S.size pubKeyBox'txs) <> " Man: " <> showt pubKeyBox'manual

dbgPrivInternalsPage :: MonadFront t m => m ()
dbgPrivInternalsPage = wrapper False "Private internal keys" (Just $ pure dbgPrivInternalsPage) $ divClass "currency-content" $ do
  void . el "div" $ retract =<< outlineButton backTxt
  buildE <- getPostBuild
  intE <- withWallet $ ffor buildE $ \_ prv -> do
    let PrvKeystore _ _ int = prv ^. prvStorage'currencyPrvStorages
          . at BTC . non (error "btcSendConfirmationWidget: not exsisting store!")
          . currencyPrvStorage'prvKeystore
    pure $ V.indexed int
  void $ networkHold (pure ()) $ ffor intE $ \ints -> divClass "" $ do
    void $ flip traverse ints $ \(i, k) -> do
      let k' = unEgvXPrvKey k
      let p = egvAddrToText $ egvXPubKeyToEgvAddress $ flip BtcXPubKey "" $ HK.deriveXPubKey k'
      el "div" $ text $ showt i <> " ------------------------------------------"
      el "div" $ text $ showt $ k'
      el "div" $ text $ p

dbgPrivExternalsPage :: MonadFront t m => m ()
dbgPrivExternalsPage = wrapper False "Private external keys" (Just $ pure dbgPrivExternalsPage) $ divClass "currency-content" $ do
  void . el "div" $ retract =<< outlineButton backTxt
  buildE <- getPostBuild
  extE <- withWallet $ ffor buildE $ \_ prv -> do
    let PrvKeystore _ ext _ = prv ^. prvStorage'currencyPrvStorages
          . at BTC . non (error "btcSendConfirmationWidget: not exsisting store!")
          . currencyPrvStorage'prvKeystore
    pure $ V.indexed ext
  void $ networkHold (pure ()) $ ffor extE $ \exts -> divClass "" $ do
    void $ flip traverse exts $ \(i, k) -> do
      let k' = unEgvXPrvKey k
      let p = egvAddrToText $ egvXPubKeyToEgvAddress $ flip BtcXPubKey "" $ HK.deriveXPubKey k'
      el "div" $ text $ showt i <> " ------------------------------------------"
      el "div" $ text $ showt $ k'
      el "div" $ text $ p

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
        el "h2" $ text $ showt $ txHash tx
        el "div" $ text $ showt tx
        el "div" $ text $ "  "
        el "div" $ text $ bs2Hex $ encode tx
        el "div" $ text "------------------------------------------------------"

dbgBlocksPage :: MonadFront t m => m ()
dbgBlocksPage = wrapper False "Blocks" (Just $ pure dbgBlocksPage) $ divClass "currency-content" $ do
  el "h2" $ text "lol"
  let requiredBlock :: HB.BlockHash = fromString "000000000000000000053331707fcbaa576b72ce41cf3f82cb22010886e9fd9c"
  el "h2" $ text $ showt $ requiredBlock
  reqE <- outlineButton $ mkTxt "Req"
  blockE <- requestBlocksBtc $ [requiredBlock] <$ reqE
  let actE = leftmost [Nothing <$ reqE, Just <$> blockE]
  networkHold (pure ()) $ ffor actE $ \case
    Nothing -> pure () 
    Just b -> divClass "" $ text $ showt $ HB.blockHeader $ head b
  pure ()
