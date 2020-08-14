module Ergvein.Wallet.Debug
  (
    debugWidget
  ) where

import Control.Lens
import Data.Maybe (fromMaybe)
import Network.Haskoin.Transaction

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Utxo
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Wrapper
import Ergvein.Index.Protocol.Types (Message(..))

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Network.Haskoin.Keys as HK
import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import qualified Ergvein.Index.Protocol.Types as IPT

data DebugBtns
  = DbgUtxo
  | DbgPubInt
  | DbgPubExt
  | DbgPrvInt
  | DbgPrvExt


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
  pingE <- outlineButton ("Ping" :: Text)
  dbgFiltersTest
  broadcastIndexerMessage $ ffor pingE $ const $ IndexerMsg $ PingMsg 123
  let goE = leftmost [utxoE, pubIntE, pubExtE, prvIntE, prvExtE]
  void $ nextWidget $ ffor goE $ \sel -> Retractable {
      retractableNext = case sel of
        DbgUtxo   -> dbgUtxoPage
        DbgPubInt -> dbgPubInternalsPage
        DbgPubExt -> dbgPubExternalsPage
        DbgPrvInt -> dbgPrivInternalsPage
        DbgPrvExt -> dbgPrivExternalsPage
    , retractablePrev = Nothing
    }

dbgUtxoPage :: MonadFront t m => m ()
dbgUtxoPage = wrapper False "UTXO" (Just $ pure dbgUtxoPage) $ divClass "currency-content" $ do
  void . el "div" $ retract =<< outlineButton backTxt
  pubSD <- getPubStorageD
  let utxoD = ffor pubSD $ \ps -> M.toList $ fromMaybe M.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (view currencyPubStorage'utxos)
  void $ widgetHoldDyn $ ffor utxoD $ \utxo -> divClass "" $ do
    flip traverse utxo $ \(o, UtxoMeta{..}) -> do
      el "div" $ text $ showt $ outPointHash o
      el "div" $ text $ showt (utxoMeta'purpose, utxoMeta'index) <> " amount: " <> showt utxoMeta'amount <> " " <> showt utxoMeta'status
      el "div" $ text $ showt utxoMeta'script
      el "div" $ text $ "------------------------------------------"
    pure ()

dbgPubInternalsPage :: MonadFront t m => m ()
dbgPubInternalsPage = wrapper False "Public internal keys" (Just $ pure dbgPubInternalsPage) $ divClass "currency-content" $ do
  void . el "div" $ retract =<< outlineButton backTxt
  pubSD <- getPubStorageD
  let intsD = ffor pubSD $ \ps -> V.indexed $ fromMaybe V.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (\a -> a ^. currencyPubStorage'pubKeystore & pubKeystore'internal)
  void $ widgetHoldDyn $ ffor intsD $ \ints -> divClass "" $ do
    flip traverse ints $ \(i, EgvPubKeyBox{..}) -> do
      let keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress pubKeyBox'key
      el "div" $ text $ showt i <> ": " <> keyTxt <> "; Txs: " <> showt (S.size pubKeyBox'txs) <> " Man: " <> showt pubKeyBox'manual

dbgPubExternalsPage :: MonadFront t m => m ()
dbgPubExternalsPage = wrapper False "Public external keys" (Just $ pure dbgPubExternalsPage) $ divClass "currency-content" $ do
  void . el "div" $ retract =<< outlineButton backTxt
  pubSD <- getPubStorageD
  let intsD = ffor pubSD $ \ps -> V.indexed $ fromMaybe V.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (\a -> a ^. currencyPubStorage'pubKeystore & pubKeystore'external)
  void $ widgetHoldDyn $ ffor intsD $ \ints -> divClass "" $ do
    flip traverse ints $ \(i, EgvPubKeyBox{..}) -> do
      let keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress pubKeyBox'key
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
  void $ widgetHold (pure ()) $ ffor intE $ \ints -> divClass "" $ do
    void $ flip traverse ints $ \(i, k) -> do
      let k' = unEgvXPrvKey k
      let p = egvAddrToString $ egvXPubKeyToEgvAddress $ flip BtcXPubKey "" $ HK.deriveXPubKey k'
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
  void $ widgetHold (pure ()) $ ffor extE $ \exts -> divClass "" $ do
    void $ flip traverse exts $ \(i, k) -> do
      let k' = unEgvXPrvKey k
      let p = egvAddrToString $ egvXPubKeyToEgvAddress $ flip BtcXPubKey "" $ HK.deriveXPubKey k'
      el "div" $ text $ showt i <> " ------------------------------------------"
      el "div" $ text $ showt $ k'
      el "div" $ text $ p

dbgFiltersTest :: MonadFront t m => m ()
dbgFiltersTest = do
  getE <- outlineButton $ mkTxt "Getfilt"
  let msg = FiltersRequestMsg $ FilterRequestMessage IPT.BTC 1 1
  respE <- requestRandomIndexer $ msg <$ getE
  let filtE = fforMaybe respE $ \case
        FiltersResponseMsg f -> Just f
        _ -> Nothing
  performEvent $ (logWrite . showt) <$> filtE
  pure ()
