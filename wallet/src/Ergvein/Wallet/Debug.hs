{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Ergvein.Wallet.Debug
  (
    debugWidget
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Network.Haskoin.Block
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
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Wrapper
import Ergvein.Index.Protocol.Types (Message(..))
import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Types.Block

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
  pingE <- outlineButton $ mkTxt "Ping"
  pingD <- indexerPingerWidget (head defaultIndexers) pingE
  h5 . dynText $ do
    p <- pingD
    pure $ "Def.indexer ping: " <> showt p
  avgD <- indexersAverageLatencyWidget =<< delay 1 pingE
  h5 . dynText $ do
    p <- avgD
    pure $ "Avg.indexer ping: " <> showt p
  -- dbgFiltersTest
  h5 . dynText . fmap showt =<< getCurrentHeight BTC
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

addUrlWidget :: forall t m . MonadFront t m => Dynamic t Bool -> m (Event t SockAddr)
addUrlWidget showD = fmap switchDyn $ widgetHoldDyn $ ffor showD $ \b -> if not b then pure never else do
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
  void $ widgetHold (pure ()) $ ffor murlE $ \case
    Nothing -> divClass "form-field-errors" $ text "Falied to parse URL"
    _ -> pure ()
  pure $ fmapMaybe id murlE

dbgUtxoPage :: MonadFront t m => m ()
dbgUtxoPage = wrapper False "UTXO" (Just $ pure dbgUtxoPage) $ divClass "currency-content" $ do
  void . el "div" $ retract =<< outlineButton backTxt
  pubSD <- getPubStorageD
  let utxoD = ffor pubSD $ \ps -> M.toList $ fromMaybe M.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (view currencyPubStorage'utxos)
  void $ widgetHoldDyn $ ffor utxoD $ \utxo -> divClass "" $ do
    void $ flip traverse utxo $ \(o, UtxoMeta{..}) -> do
      el "div" $ text $ showt $ outPointHash o
      el "div" $ text $ showt (utxoMeta'purpose, utxoMeta'index) <> " amount: " <> showt utxoMeta'amount <> " " <> showt utxoMeta'status
      el "div" $ text $ showt utxoMeta'script
      el "div" $ text $ "------------------------------------------"

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
  getE <- outlineButton $ mkTxt "dbgFiltersTest"
  filtsE <- getFilters BTC $ (1, 10) <$ getE
  performEvent_ $ ffor filtsE $ \filts -> void $ flip traverse filts $ \(bh, filt) -> do
    logWrite "====================================="
    logWrite $ showt bh
    logWrite filt

getFilters :: MonadFront t m => Currency -> Event t (BlockHeight, Int) -> m (Event t [(BlockHash, AddressFilterHexView)])
getFilters cur e = do
  respE <- requestRandomIndexer $ ffor e $ \(h, n) ->
    MFiltersRequest $ FilterRequest curcode (fromIntegral h) (fromIntegral n)
  pure $ fforMaybe respE $ \case
    MFiltersResponse (FilterResponse{..}) -> if filterResponseCurrency /= curcode
      then Nothing
      else Just $ catMaybes $ V.toList $ ffor filterResponseFilters $ \(BlockFilter bid filt) -> let
        mbh = hexToBlockHash $ bs2Hex bid
        fview = bs2Hex filt
        in (, fview) <$> mbh
    _ -> Nothing
  where
    curcode = currencyToCurrencyCode cur
