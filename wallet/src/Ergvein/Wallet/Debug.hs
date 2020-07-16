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
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Network.Haskoin.Keys as HK

data DebugBtns
  = DbgUtxo
  | DbgPubInt
  | DbgPubExt
  | DbgPrvInt
  | DbgPrvExt


debugWidget :: MonadFront t m => m ()
debugWidget = el "div" $ do
  utxoE <- fmap (DbgUtxo <$) $ outlineButton ("UTXO" :: Text)
  pubIntE <- fmap (DbgPubInt <$) $ outlineButton ("Pub Internals" :: Text)
  pubExtE <- fmap (DbgPubExt <$) $ outlineButton ("Pub Externals" :: Text)
  prvIntE <- fmap (DbgPrvInt <$) $ outlineButton ("Priv Internals" :: Text)
  prvExtE <- fmap (DbgPrvExt <$) $ outlineButton ("Priv Externals" :: Text)
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
  pubSD <- getPubStorageD
  let utxoD = ffor pubSD $ \ps -> M.toList $ fromMaybe M.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (view currencyPubStorage'utxos)
  widgetHoldDyn $ ffor utxoD $ \utxo -> divClass "" $ do
    flip traverse utxo $ \(o, UtxoMeta{..}) -> do
      el "div" $ text $ showt $ outPointHash o
      el "div" $ text $ showt (utxoMeta'purpose, utxoMeta'index) <> " amount: " <> showt utxoMeta'amount <> " " <> showt utxoMeta'status
      el "div" $ text $ showt utxoMeta'script
      el "div" $ text $ "------------------------------------------"
    pure ()
  void . el "div" $ retract =<< outlineButton ("Back" :: Text)

dbgPubInternalsPage :: MonadFront t m => m ()
dbgPubInternalsPage = wrapper False "Public internal keys" (Just $ pure dbgPubInternalsPage) $ divClass "currency-content" $ do
  pubSD <- getPubStorageD
  let intsD = ffor pubSD $ \ps -> V.indexed $ fromMaybe V.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (\a -> a ^. currencyPubStorage'pubKeystore & pubKeystore'internal)
  widgetHoldDyn $ ffor intsD $ \ints -> divClass "" $ do
    flip traverse ints $ \(i, EgvPubKeyBox{..}) -> do
      let keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress pubKeyBox'key
      el "div" $ text $ showt i <> ": " <> keyTxt <> "; Txs: " <> showt (S.size pubKeyBox'txs) <> " Man: " <> showt pubKeyBox'manual
  void . el "div" $ retract =<< outlineButton ("Back" :: Text)

dbgPubExternalsPage :: MonadFront t m => m ()
dbgPubExternalsPage = wrapper False "Public external keys" (Just $ pure dbgPubExternalsPage) $ divClass "currency-content" $ do
  pubSD <- getPubStorageD
  let intsD = ffor pubSD $ \ps -> V.indexed $ fromMaybe V.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (\a -> a ^. currencyPubStorage'pubKeystore & pubKeystore'external)
  widgetHoldDyn $ ffor intsD $ \ints -> divClass "" $ do
    flip traverse ints $ \(i, EgvPubKeyBox{..}) -> do
      let keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress pubKeyBox'key
      el "div" $ text $ showt i <> ": " <> keyTxt <> "; Txs: " <> showt (S.size pubKeyBox'txs) <> " Man: " <> showt pubKeyBox'manual
  void . el "div" $ retract =<< outlineButton ("Back" :: Text)

dbgPrivInternalsPage :: MonadFront t m => m ()
dbgPrivInternalsPage = wrapper False "Private internal keys" (Just $ pure dbgPrivInternalsPage) $ divClass "currency-content" $ do
  buildE <- getPostBuild
  intE <- withWallet $ ffor buildE $ \_ prv -> do
    let PrvKeystore _ _ int = prv ^. prvStorage'currencyPrvStorages
          . at BTC . non (error "btcSendConfirmationWidget: not exsisting store!")
          . currencyPrvStorage'prvKeystore
    pure $ V.indexed int
  widgetHold (pure ()) $ ffor intE $ \ints -> divClass "" $ do
    void $ flip traverse ints $ \(i, k) -> do
      let k' = unEgvXPrvKey k
      let p = egvAddrToString $ egvXPubKeyToEgvAddress $ flip BtcXPubKey "" $ HK.deriveXPubKey k'
      el "div" $ text $ showt i <> " ------------------------------------------"
      el "div" $ text $ showt $ k'
      el "div" $ text $ p
      pure ()
  pure ()


dbgPrivExternalsPage :: MonadFront t m => m ()
dbgPrivExternalsPage = wrapper False "Private external keys" (Just $ pure dbgPrivExternalsPage) $ divClass "currency-content" $ do
  buildE <- getPostBuild
  extE <- withWallet $ ffor buildE $ \_ prv -> do
    let PrvKeystore _ ext _ = prv ^. prvStorage'currencyPrvStorages
          . at BTC . non (error "btcSendConfirmationWidget: not exsisting store!")
          . currencyPrvStorage'prvKeystore
    pure $ V.indexed ext
  widgetHold (pure ()) $ ffor extE $ \exts -> divClass "" $ do
    void $ flip traverse exts $ \(i, k) -> do
      let k' = unEgvXPrvKey k
      let p = egvAddrToString $ egvXPubKeyToEgvAddress $ flip BtcXPubKey "" $ HK.deriveXPubKey k'
      el "div" $ text $ showt i <> " ------------------------------------------"
      el "div" $ text $ showt $ k'
      el "div" $ text $ p
  pure ()
