{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.History(
    historyPage
  ) where

import Control.Lens.Combinators
import Control.Monad.Reader

import Ergvein.Filters.Btc
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Blocks.BTC.Queries
import Ergvein.Wallet.Blocks.Types
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.History
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Tx
import Ergvein.Wallet.Worker.Node
import Ergvein.Wallet.Wrapper

import Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.List as L
import Network.Haskoin.Address
import Data.Maybe (fromMaybe, isJust)
import Data.Time
import Data.Text as T
import Data.Serialize

import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Constants          as HK
import qualified Network.Haskoin.Script             as HK
import qualified Network.Haskoin.Transaction        as HK
import qualified Network.Haskoin.Util               as HK

historyPage :: MonadFront t m => Currency -> m ()
historyPage cur = wrapper False (HistoryTitle cur) (Just $ pure $ historyPage cur) $ do
  ps <- getPubStorage
  pubSD <- getPubStorageD
  let thisWidget = Just $ pure $ historyPage cur
      historyWidget = historyTableWidget cur $ mockTransHistory cur
  navbarWidget cur thisWidget NavbarHistory
  goE <- historyWidget
  void $ nextWidget $ ffor goE $ \tr -> Retractable {
      retractableNext = transactionInfoPage cur tr
    , retractablePrev = thisWidget
    }

#ifdef ANDROID
transactionInfoPage :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPage cur tr@TransactionView{..} = wrapper False HistoryTITitle (Just $ pure $ transactionInfoPage cur tr) $ mdo
  hashD <- expD hashE hashD
  hashE <- expHead hashD HistoryTIHash
  copiedHashE <- copyDiv hashD $ txId txInfoView
  case (txLabel txInfoView) of
    Just lbl -> do
      divClass "info-descr-andr" $ localizedText HistoryTILabel
      divClass "info-andr-element" $ do
        divClass "info-body info-label" $ text lbl
    Nothing -> pure ()
  divClass "info-descr-andr" $ localizedText HistoryTIVolume
  divClass "info-body-andr" $ do
    text $ showMoney $ txAmount
    elClass "span" "currname" $ text $ showt cur
  divClass "info-descr-andr" $ localizedText HistoryTIFee
  divClass "info-body-andr" $ do
    text $ showMoney $ txFee txInfoView
    elClass "span" "currname" $ text $ showt cur
  divClass "info-descr-andr" $ localizedText HistoryTIConfirmations
  divClass "info-body-andr" $ do
    text $ showt $ txConfirmations txInfoView

  blockD <- expD blockE blockD
  blockE <- expHead blockD HistoryTIBlock
  copiedBlockE <- copyDiv blockD $ txBlock txInfoView

  rawD <- expD rawE rawD
  rawE <- expHead rawD HistoryTIRaw
  copiedRawE <- copyDiv rawD $ txRaw txInfoView

  divClass "info-descr-andr" $ localizedText HistoryTIOutputs
  divClass "info-body-andr info-exits-andr" $ do
    flip traverse (txOutputs txInfoView) $ \(oHash,oVal,oType) -> divClass "out-element" $ do
      divClass "out-descr-andr" $ localizedText HistoryTIOutputsValue
      divClass "out-body-andr"  $ do
        text $ showMoney $ oVal
        text $ showt cur
      divClass "out-descr-andr" $ localizedText HistoryTIOutputsAddress
      divClass "out-body-andr"  $ text $ oHash
      divClass "out-descr-andr" $ localizedText HistoryTIOutputsStatus
      divClass "out-body-andr"  $ localizedText oType
  divClass "info-descr-andr" $ localizedText HistoryTIInputs
  divClass "info-body-andr info-exits-andr" $ do
    flip traverse (txInputs txInfoView) $ \(oHash,oVal) -> divClass "out-element" $ do
      divClass "out-descr-andr" $ localizedText HistoryTIOutputsValue
      divClass "out-body-andr" $ do
        text $ showMoney $ oVal
        text $ showt cur
      divClass "out-descr-andr-anrd" $ localizedText HistoryTIOutputsAddress
      divClass "out-body-andr"  $ text $ oHash
  divClass "info-descr-andr" $ localizedText HistoryTIURL
  divClass "info-andr-element" $ do
    divClass "info-body-andr info-url" $ hyperlink "link" (txUrl txInfoView) (txUrl txInfoView)
  let copiedE = leftmost[(txId txInfoView) <$ copiedHashE,
                          (txBlock txInfoView) <$ copiedBlockE,
                          (txRaw txInfoView) <$ copiedRawE]
  cE <- clipboardCopy $ copiedE
  showSuccessMsg $ CSCopied <$ cE
  pure ()
  where
    expD expE expD = holdDyn Hidden $ poke expE $ \_ -> do
          st <- sampleDyn expD
          case st of
            Hidden -> pure Expanded
            Expanded -> pure Hidden

    expDiv statD txt = widgetHoldDyn $ ffor statD $ \exStatus -> case exStatus of
      Hidden   -> divClass "info-body-andr" $ text txt
      Expanded -> divClass "info-body-andr-expanded" $ text txt

    expHead statD txt = divButton "info-descr-andr" $ do
        localizedText txt
        elClass "span" "expand-button" $ widgetHoldDyn $ ffor statD $ \exStatus -> case exStatus of
          Hidden   -> text $ "▼"
          Expanded -> text $ "▲"  -- ▼▲ ▾▴

    copyDiv copyD txt = divButton "info-hash-andr info-andr-element" $ do
      expDiv copyD txt
      divClass "info-copy-button" $ text $ ""
#else
transactionInfoPage :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPage cur tr@TransactionView{..} = wrapper False HistoryTITitle (Just $ pure $ transactionInfoPage cur tr) $ do
  divClass "tx-info-page-element" $ do
    divClass "info-descr" $ localizedText HistoryTIHash
    divClass "info-body info-hash" $ text $ txId txInfoView
  case (txLabel txInfoView) of
    Just lbl -> do
      divClass "tx-info-page-element" $ do
        divClass "info-descr" $ localizedText HistoryTILabel
        divClass "info-body info-label" $ text lbl
    Nothing -> pure ()
  divClass "tx-info-page-element" $ do
    let url = txUrl txInfoView
    divClass "info-descr " $ localizedText HistoryTIURL
    divClass "info-body info-url" $ hyperlink "link" url url
  divClass "tx-info-page-element" $ do
    divClass "info-descr" $ localizedText HistoryTIVolume
    divClass "info-body info-fee" $ do
      text $ showMoney $ txAmount
      elClass "span" "currname" $ text $ showt cur
  divClass "tx-info-page-element" $ do
    divClass "info-descr" $ localizedText HistoryTIFee
    divClass "info-body info-fee" $ do
      text $ showMoney $ txFee txInfoView
      elClass "span" "currname" $ text $ showt cur
  divClass "tx-info-page-element" $ do
    divClass "info-descr" $ localizedText HistoryTIConfirmations
    divClass "info-body info-conf" $ text $ showt $ txConfirmations txInfoView
  divClass "tx-info-page-element" $ do
    divClass "info-descr" $ localizedText HistoryTIBlock
    divClass "info-body info-block" $ text $ txBlock txInfoView
  divClass "tx-info-page-element" $ do
    divClass "info-descr" $ localizedText HistoryTIRaw
    divClass "info-body info-raw" $ text $ txRaw txInfoView
  divClass "tx-info-page-element" $ do
    divClass "info-descr" $ localizedText HistoryTIOutputs
    divClass "info-body info-out" $ do
      flip traverse (txOutputs txInfoView) $ \(oHash,oVal,oType) -> divClass "out-element" $ do
        divClass "out-descr" $ localizedText HistoryTIOutputsValue
        divClass "out-body"  $ do
          text $ showMoney $ oVal
          elClass "span" "currname" $ text $ showt cur
        divClass "out-descr" $ localizedText HistoryTIOutputsAddress
        divClass "out-body"  $ text $ oHash
        divClass "out-descr" $ localizedText HistoryTIOutputsStatus
        divClass "out-body"  $ localizedText oType
  divClass "tx-info-page-element" $ do
    divClass "info-descr" $ localizedText HistoryTIInputs
    divClass "info-body info-in" $ do
      flip traverse (txInputs txInfoView) $ \(oHash,oVal) -> divClass "out-element" $ do
        divClass "out-descr" $ localizedText HistoryTIOutputsValue
        divClass "out-body" $ do
          text $ showMoney $ oVal
          elClass "span" "currname" $ text $ showt cur
        divClass "out-descr" $ localizedText HistoryTIOutputsAddress
        divClass "out-body"  $ text $ oHash
  pure ()
#endif

historyTableWidget :: MonadFront t m => Currency -> [TransactionView] -> m (Event t TransactionView)
historyTableWidget cur trList = case cur of
  BTC -> do
    txsD <- transactionsGetting BTC
    txClickDE <- widgetHoldDyn $ ffor txsD $ \txs -> do
      txClickE <- traverse historyTableRow txs
      pure $ leftmost txClickE
    pure $ switchDyn txClickDE
  ERGO -> do
    txClickE <- traverse historyTableRow trList
    pure $ leftmost txClickE
  where
    historyTableRow :: MonadFront t m => TransactionView -> m (Event t TransactionView)
    historyTableRow tr@TransactionView{..} = divButton "history-table-row" $ do
      divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ symb $ text $ showMoney txAmount
      divClass "history-date" $ text $ txDate
      divClass ("history-status-" <> ((T.toLower . showt) txInOut)) $ stat
      pure tr
      where
        symb :: MonadFront t m => m a -> m a
        symb ma = case txInOut of
          TransRefill -> do
            spanClass "history-page-sign-icon" $ elClass "i" "fas fa-plus fa-fw" blank
            ma
          TransWithdraw -> do
            spanClass "history-page-sign-icon" $ elClass "i" "fas fa-minus fa-fw" blank
            ma
        stat :: MonadFront t m => m ()
        stat = case txStatus of
          TransConfirmed -> spanClass "history-page-status-icon" $ elClass "i" "fas fa-check fa-fw" $ blank
          TransUncofirmed -> spanClass "history-page-status-icon" $ elClass "i" "fas fa-question fa-fw" $ blank

transactionsGetting :: MonadFront t m => Currency -> m (Dynamic t [TransactionView])
transactionsGetting cur = do
  buildE <- delay 0.2 =<< getPostBuild
  ps <- getPubStorage
  pubSD <- getPubStorageD
  let allBtcAddrsD = ffor pubSD $ \(PubStorage _ cm _ _) -> case Map.lookup BTC cm of
        Nothing -> []
        Just CurrencyPubStorage{..} -> extractAddrs _currencyPubStorage'pubKeystore

  abS <- filtArd <$> sampleDyn allBtcAddrsD
  store <- getBlocksStorage
  let rawTxList = filterTx abS ps

  hD <- holdDyn rawTxList $ poke (updated pubSD) $ \pbs -> do
    allbtcAdrS <- filtArd <$> sampleDyn allBtcAddrsD
    pure $ filterTx allbtcAdrS pbs

  filtrTxListSE <- performFork $ ffor buildE $ \_ -> do
    let tx = filterTx abS ps
    getAndFilterBlocks allBtcAddrsD tx store

  filtrTxListE <- performFork $ ffor (updated hD) $ \tx -> do
    getAndFilterBlocks allBtcAddrsD tx store

  sD <- holdDyn [] filtrTxListSE
  hS <- sampleDyn $ sD

  filtrHD <- holdDyn [] $ leftmost [filtrTxListSE, filtrTxListE]
  pure filtrHD
  where
    getAndFilterBlocks btcAddrsD tx store = do
      allbtcAdrS <- filtArd <$> sampleDyn btcAddrsD
      liftIO $ flip runReaderT store $ do
        blh <- traverse getBtcBlockHashByTxHash $ fmap HK.txHash $ fmap (getBtcTx) tx
        bl <- traverse getBlockFromHash blh
        b <- traverse (checkAddr allbtcAdrS) tx
        let txRefList = fmap (calcRefill (fmap getBtcAddr allbtcAdrS)) tx
        pure $ fmap snd $ L.filter fst $ L.zip b (prepareTransactionView <$> txListRaw bl blh tx txRefList)

    filterTx ac pubS = case cur of
      BTC  -> fmap snd $ fromMaybe [] $ fmap Map.toList $ _currencyPubStorage'transactions <$> Map.lookup cur (_pubStorage'currencyPubStorages pubS)
      ERGO -> []

    calcRefill ac tx = case tx of
        BtcTx btx _ -> Money cur $ sum $ fmap (HK.outValue . snd) $ L.filter (maybe False (flip elem ac . fromSegWit) . fst) $ fmap (\txo -> (getSegWitAddr txo,txo)) $ HK.txOut btx
        ErgTx etx _ -> Money cur 0

    checkAddr :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => [EgvAddress] -> EgvTx -> m Bool
    checkAddr ac tx = do
      bL <- traverse (flip checkAddrTx (getBtcTx tx)) ac
      pure $ L.or bL

    filtArd :: [(Maybe Int, EgvAddress)] -> [EgvAddress]
    filtArd madr = fmap snd $ L.filter (isJust . fst) madr

    txListRaw (a:as) (b:bs) (c:cs) (d:ds) = (TxRawInfo a b c d) : txListRaw as bs cs ds

    getBlockFromHash :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => Maybe HK.BlockHash -> m (Maybe HK.Block)
    getBlockFromHash mBlockHash = case mBlockHash of
      Nothing -> pure Nothing
      Just blockHash -> do
        mBlock <- getBtcBlock blockHash
        pure mBlock

prepareTransactionView :: TxRawInfo -> TransactionView
prepareTransactionView TxRawInfo{..} = TransactionView {
    txAmount = txom
  , txDate = "pending..."
  , txInOut = TransRefill
  , txInfoView = txInf
  , txStatus = TransUncofirmed
  }
  where
    txInf = TransactionViewInfo {
      txId            = txHex
     ,txLabel         = Nothing
     ,txUrl           = if isTestnet
       then "https://www.blockchain.com/btc-testnet/tx/" <> txHex
       else "https://www.blockchain.com/btc/tx/" <> txHex
     ,txFee           = Money BTC 0
     ,txConfirmations = 0
     ,txBlock         = blockHash
     ,txRaw           = btcTxToString btx
     ,txOutputs       = txOuts
     ,txInputs        = []
    }
    btx = getBtcTx txr
    txHs = HK.txHash btx
    txHex = HK.txHashToHex txHs
    txOuts = fmap (\out -> (txOutAdr out,Money BTC (HK.outValue out), TOUnspent)) $ HK.txOut btx
    txOutAdr out = maybe "undefined" (btcAddrToString . fromSegWit) $ getSegWitAddr out
    blockHash = maybe "undefined" HK.blockHashToHex txHBl


-- Front types, should be moved to Utils
data ExpStatus = Expanded | Hidden deriving (Eq, Show)

-- Mock Transaction info types for visualisation.
data TransStatus = TransConfirmed | TransUncofirmed deriving (Eq,Show)
data TransType = TransRefill | TransWithdraw deriving (Eq,Show)
data TransOutputType = TOSpent | TOUnspent deriving (Eq,Show)

instance LocalizedPrint TransOutputType where
  localizedShow l v = case l of
    English -> case v of
      TOSpent   -> "Spent"
      TOUnspent -> "Unspent"
    Russian -> case v of
      TOSpent   -> "Потрачены"
      TOUnspent -> "Непотрачены"

mockTransHistory :: Currency -> [TransactionView]
mockTransHistory cur = [
  TransactionView (moneyFromRational cur 0.63919646) "2020-04-07 14:12" TransRefill   (trMockInfo cur) TransUncofirmed
 ,TransactionView (moneyFromRational cur 0.20134303) "2020-04-07 12:30" TransWithdraw (trMockInfo cur) TransConfirmed
 ,TransactionView (moneyFromRational cur 0.40213010) "2020-04-03 10:30" TransRefill   (trMockInfo cur) TransConfirmed
 ,TransactionView (moneyFromRational cur 0.02142302) "2020-03-20 22:40" TransWithdraw (trMockInfo cur) TransConfirmed
 ,TransactionView (moneyFromRational cur 0.10024245) "2020-03-05 09:05" TransRefill   (trMockInfo cur) TransConfirmed]

trMockInfo :: Currency -> TransactionViewInfo
trMockInfo cur = TransactionViewInfo
  "330ce5f20e63b97604fb6add4e4be53197363ac5ebf7342e1372212b7b49498e"
  (Just "s3")
  "https://www.blockchain.com/btc/tx/330ce5f20e63b97604fb6add4e4be53197363ac5ebf7342e1372212b7b49498e"
  (moneyFromRational cur 0.000706)
  11
  "00000000000000000005119aeee8d2550c5875ff0569583d0ca543ed0c06b2d4"
  "010000000001033a049aac743259959d6202adbd69e533d0107f83ca21f278e2514ed3b160a80f03000000171600144ba622ca5a3babce0198b4b575ae5e23fbafa04affffffff6d5734b3d1289aaad6a6edce643c0d8c1d32408e4122c7915c4c979a100a377b0700000017160014d0050eb0fa0afa74876883971d194843aef8c3b8fffffffff7d510428b37b663a744b4e37d0c16dfaaffe2386dbb5fb5c77b6780acff4e8e0100000017160014d0050eb0fa0afa74876883971d194843aef8c3b8ffffffff024ffb49000000000017a914da2e37a0ac8f61fc60833fe4eb82f619992dc42887cf5a8503000000001976a9144eda7a74a3712d81fb0167d97e1119d673edf87b88ac02483045022100c687cb59f9d49e2b086a7fc17074f87612be3b7865e63add8f709384db8b34eb022016789c26d323c57acd590a6ad6db959d89cc9fbb8478b396e551666316264619012102f2246a7f2dd810498aa4f18fe86a6aef1e7856634d18e785b1eeda32712df96f02473044022004c6f0e07ee78f1bee6d20991ca1d237a139463d9612ca3df752296b3f1576c102205fd8d26e4c911aed931e0c0f8c006c0a94b13161d9a815a4809b525d3ae3754b012102e20a411a55d4e399dda618529c95b5a4fcdbd861dba466b38aa04e5b8f7324880247304402206ed672ce4e4b5db99461375afffcb393a29c8a5a959f21a3eb5a97b1bd4e4c3902204bab74dfca7b97f965c37ff34a6462aa391609f3793c561db27e141b312610f7012102e20a411a55d4e399dda618529c95b5a4fcdbd861dba466b38aa04e5b8f73248800000000"
  [("3MaebbZnWMXoxTWR7SHVGS3W6Xuw5FU164",(moneyFromRational cur 0.04848463),TOUnspent),("18BwS73Fq7D5HY8rGkYCsWNGXXRfEvDxW2",(moneyFromRational cur 0.59071183),TOSpent)]
  [("3Mx9XH35FrbpVjsDayKyvc6eSDZfjJAsx5",(moneyFromRational cur 0.13282286)),("3EVkfRx1cPWC8czue1RH4d6rTXghWyCXDS",(moneyFromRational cur 0.25622085)),("3EVkfRx1cPWC8czue1RH4d6rTXghWyCXDS",(moneyFromRational cur 0.25085875))]

data TxRawInfo = TxRawInfo {
  txMBl :: Maybe HK.Block
 ,txHBl :: Maybe HK.BlockHash
 ,txr   :: EgvTx
 ,txom  :: Money
} deriving (Show)

data TransactionView = TransactionView {
  txAmount   :: Money
 ,txDate     :: Text
 ,txInOut    :: TransType
 ,txInfoView :: TransactionViewInfo
 ,txStatus   :: TransStatus
} deriving (Show)

data TransactionViewInfo = TransactionViewInfo {
  txId            :: Text
 ,txLabel         :: Maybe Text
 ,txUrl           :: Text
 ,txFee           :: Money
 ,txConfirmations :: Int
 ,txBlock         :: Text
 ,txRaw           :: Text
 ,txOutputs       :: [(Text,Money,TransOutputType)]
 ,txInputs        :: [(Text,Money)]
} deriving (Show)

{-
data BlockMetaInfo = BlockMetaInfo
  { blockMetaCurrency      :: Currency
  , blockMetaBlockHeight   :: BlockHeight
  , blockMetaHeaderHashHexView :: BlockHeaderHashHexView
  , blockMetaAddressFilterHexView :: AddressFilterHexView
  } deriving (Show)

data TxInfo = TxInfo
  { txHash         :: TxHash
  , txHexView      :: TxHexView
  , txOutputsCount :: Word
  } deriving (Show)

data BlockInfo = BlockInfo
  { blockInfoMeta       :: BlockMetaInfo
  , spentTxsHash        :: [TxHash]
  , blockContentTxInfos :: [TxInfo]
  } deriving (Show)
-}

{-
txInfo :: HK.Tx -> ([TxInfo], [TxHash])
txInfo tx = let
  withoutDataCarrier = none HK.isDataCarrier . HK.decodeOutputBS . HK.scriptOutput
  info = TxInfo { txHash = HK.txHashToHex $ txHash tx
                , txHexView = HK.encodeHex $ encode tx
                , txOutputsCount = fromIntegral $ L.length $ L.filter withoutDataCarrier $  HK.txOut tx
                }
  withoutCoinbaseTx = L.filter $ (/= HK.nullOutPoint)
  spentTxInfo = HK.txHashToHex . HK.outPointHash <$> (withoutCoinbaseTx $ HK.prevOutput <$> HK.txIn tx)
  in ([info], spentTxInfo)
-}
