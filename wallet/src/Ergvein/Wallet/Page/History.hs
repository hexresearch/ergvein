{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.History(
    historyPage
  ) where

import Control.Monad.Reader

import Ergvein.Filters.Btc
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Alert
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
import Ergvein.Wallet.Tx
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Worker.Node

import qualified Data.Map as M
import Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Network.Haskoin.Transaction
import Network.Haskoin.Address
import Data.Time
import Data.Text as T
import Data.Maybe (fromMaybe)

historyPage :: MonadFront t m => Currency -> m ()
historyPage cur = wrapper (HistoryTitle cur) (Just $ pure $ historyPage cur) False $ do
  ps <- getPubStorage
  pubSD <- getPubStorageD
  let thisWidget = Just $ pure $ historyPage cur
      historyWidget = case cur of
        BTC  -> divClass "history-table-body" $ historyTableWidget $ mockTransHistory cur
        ERGO -> divClass "history-table-body" $ historyTableWidget $ mockTransHistory cur
  navbarWidget cur thisWidget NavbarHistory
  goE <- historyWidget
  void $ nextWidget $ ffor (leftmost goE) $ \tr -> Retractable {
      retractableNext = transactionInfoPage cur tr
    , retractablePrev = thisWidget
    }

#ifdef ANDROID
transactionInfoPage :: MonadFront t m => Currency -> TransactionMock -> m ()
transactionInfoPage cur tr@TransactionMock{..} = wrapper HistoryTITitle (Just $ pure $ transactionInfoPage cur tr) False $ do
  divClass "transaction-info-body-andr" $ mdo
    hashD <- expD hashE hashD
    hashE <- expHead hashD HistoryTIHash
    copiedHashE <- copyDiv hashD $ txId txInfo
    case (txLabel txInfo) of
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
      text $ showMoney $ txFee txInfo
      elClass "span" "currname" $ text $ showt cur
    divClass "info-descr-andr" $ localizedText HistoryTIConfirmations
    divClass "info-body-andr" $ do
      text $ showt $ txConfirmations txInfo

    blockD <- expD blockE blockD
    blockE <- expHead blockD HistoryTIBlock
    copiedBlockE <- copyDiv blockD $ txBlock txInfo

    rawD <- expD rawE rawD
    rawE <- expHead rawD HistoryTIRaw
    copiedRawE <- copyDiv rawD $ txRaw txInfo

    divClass "info-descr-andr" $ localizedText HistoryTIOutputs
    divClass "info-body-andr info-exits-andr" $ do
      flip traverse (txOutputs txInfo) $ \(oHash,oVal,oType) -> divClass "out-element" $ do
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
      flip traverse (txInputs txInfo) $ \(oHash,oVal) -> divClass "out-element" $ do
        divClass "out-descr-andr" $ localizedText HistoryTIOutputsValue
        divClass "out-body-andr" $ do
          text $ showMoney $ oVal
          text $ showt cur
        divClass "out-descr-andr-anrd" $ localizedText HistoryTIOutputsAddress
        divClass "out-body-andr"  $ text $ oHash
    divClass "info-descr-andr" $ localizedText HistoryTIURL
    divClass "info-andr-element" $ do
      divClass "info-body-andr info-url" $ hyperlink "link" (txUrl txInfo) (txUrl txInfo)
    let copiedE = leftmost[(txId txInfo) <$ copiedHashE,
                           (txBlock txInfo) <$ copiedBlockE,
                           (txRaw txInfo) <$ copiedRawE]
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
transactionInfoPage :: MonadFront t m => Currency -> TransactionMock -> m ()
transactionInfoPage cur tr@TransactionMock{..} = wrapper HistoryTITitle (Just $ pure $ transactionInfoPage cur tr) False $ do
  divClass "transaction-info-body" $ do
    divClass "transaction-info-element" $ do
      divClass "info-descr" $ localizedText HistoryTIHash
      divClass "info-body info-hash" $ text $ txId txInfo
    case (txLabel txInfo) of
      Just lbl -> do
        divClass "transaction-info-element" $ do
          divClass "info-descr" $ localizedText HistoryTILabel
          divClass "info-body info-label" $ text lbl
      Nothing -> pure ()
    divClass "transaction-info-element" $ do
      let url = txUrl txInfo
      divClass "info-descr " $ localizedText HistoryTIURL
      divClass "info-body info-url" $ hyperlink "link" url url
    divClass "transaction-info-element" $ do
      divClass "info-descr" $ localizedText HistoryTIVolume
      divClass "info-body info-fee" $ do
        text $ showMoney $ txAmount
        elClass "span" "currname" $ text $ showt cur
    divClass "transaction-info-element" $ do
      divClass "info-descr" $ localizedText HistoryTIFee
      divClass "info-body info-fee" $ do
        text $ showMoney $ txFee txInfo
        elClass "span" "currname" $ text $ showt cur
    divClass "transaction-info-element" $ do
      divClass "info-descr" $ localizedText HistoryTIConfirmations
      divClass "info-body info-conf" $ text $ showt $ txConfirmations txInfo
    divClass "transaction-info-element" $ do
      divClass "info-descr" $ localizedText HistoryTIBlock
      divClass "info-body info-block" $ text $ txBlock txInfo
    divClass "transaction-info-element" $ do
      divClass "info-descr" $ localizedText HistoryTIRaw
      divClass "info-body info-raw" $ text $ txRaw txInfo
    divClass "transaction-info-element" $ do
      divClass "info-descr" $ localizedText HistoryTIOutputs
      divClass "info-body info-out" $ do
        flip traverse (txOutputs txInfo) $ \(oHash,oVal,oType) -> divClass "out-element" $ do
          divClass "out-descr" $ localizedText HistoryTIOutputsValue
          divClass "out-body"  $ do
            text $ showMoney $ oVal
            elClass "span" "currname" $ text $ showt cur
          divClass "out-descr" $ localizedText HistoryTIOutputsAddress
          divClass "out-body"  $ text $ oHash
          divClass "out-descr" $ localizedText HistoryTIOutputsStatus
          divClass "out-body"  $ localizedText oType
    divClass "transaction-info-element" $ do
      divClass "info-descr" $ localizedText HistoryTIInputs
      divClass "info-body info-in" $ do
        flip traverse (txInputs txInfo) $ \(oHash,oVal) -> divClass "out-element" $ do
          divClass "out-descr" $ localizedText HistoryTIOutputsValue
          divClass "out-body" $ do
            text $ showMoney $ oVal
            elClass "span" "currname" $ text $ showt cur
          divClass "out-descr" $ localizedText HistoryTIOutputsAddress
          divClass "out-body"  $ text $ oHash
  pure ()
#endif

historyTableWidget :: MonadFront t m => [TransactionMock] -> m ([Event t TransactionMock])
historyTableWidget trList = do
   txsD <- transactionsGetting BTC
   txClickE <- traverse historyTableRow trList
   pure txClickE
    where
      historyTableRow :: MonadFront t m => TransactionMock -> m (Event t TransactionMock)
      historyTableRow tr@TransactionMock{..} = divButton "history-table-row" $ do
        divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ text $ symb <> (showMoney txAmount)
        divClass "history-date"   $ text $ txDate
        divClass ("history-status-" <> ((T.toLower . showt) txInOut)) $ text $ stat
        pure tr
        where
          symb :: Text
          symb = case txInOut of
            TransRefill   -> "+"
            TransWithdraw -> "-"
          stat :: Text
          stat = case txStatus of
            TransConfirmed -> "✓"
            TransUncofirmed -> "?"

transactionsGetting :: MonadFront t m => Currency -> m (Dynamic t [EgvTx])
transactionsGetting cur = do
  buildE <- delay 0.2 =<< getPostBuild
  ps <- getPubStorage
  pubSD <- getPubStorageD
  let allBtcAddrsD = ffor pubSD $ \(PubStorage _ cm _) -> case M.lookup BTC cm of
        Nothing -> []
        Just (CurrencyPubStorage keystore txmap) -> extractAddrs keystore
  abS <- filtArd <$> sampleDyn allBtcAddrsD
  store <- getBlocksStorage
  let rawTxList = calcSum abS ps

  hD <- holdDyn rawTxList $ poke (updated pubSD) $ \pbs -> do
    allbtcAdrS <- filtArd <$> sampleDyn allBtcAddrsD
    pure $ calcSum allbtcAdrS pbs

  filtrTxListE <- performFork $ ffor (updated hD) $ \tx -> do
    allbtcAdrS <- filtArd <$> sampleDyn allBtcAddrsD
    liftIO $ flip runReaderT store $ do
      b <- traverse (checkAddr allbtcAdrS) tx
      pure $ fmap snd $ L.filter fst $ L.zip b tx
  hS <- sampleDyn hD
  filtrHD <- holdDyn hS filtrTxListE
  pure filtrHD
  --txfiltList <- L.filter (\tx -> checkAddr allbtcAdrS tx) txList
  where
    calcSum ac pubS = case cur of
      BTC  -> fmap snd $ fromMaybe [] $ fmap Map.toList $ _currencyPubStorage'transactions <$> Map.lookup cur (_pubStorage'currencyPubStorages pubS)
      ERGO -> []

    checkAddr :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => [EgvAddress] -> EgvTx -> m Bool
    checkAddr ac tx = do
      bL <- traverse (flip checkAddrTx (getBtcTx tx)) ac
      pure $ L.or bL

    filtArd :: [(Maybe Int, EgvAddress)] -> [EgvAddress]
    filtArd madr = fmap snd $ L.filter (\(mi,b) -> case mi of
      Nothing -> False
      Just mi -> True) madr

    gbA s = fmap (\(_,a) -> getBtcAddr a) s

historyTableWidget2 :: MonadFront t m => [TransactionMock] -> m ([Event t TransactionMock])
historyTableWidget2 trList = do
   traverse historyTableRow trList
    where
      historyTableRow :: MonadFront t m => TransactionMock -> m (Event t TransactionMock)
      historyTableRow tr@TransactionMock{..} = divButton "history-table-row" $ do
        divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ text $ symb <> (showMoney txAmount)
        divClass "history-date"   $ text $ txDate
        divClass ("history-status-" <> ((T.toLower . showt) txInOut)) $ text $ stat
        pure tr
        where
          symb :: Text
          symb = case txInOut of
            TransRefill   -> "+"
            TransWithdraw -> "-"
          stat :: Text
          stat = case txStatus of
            TransConfirmed -> "✓"
            TransUncofirmed -> "?"

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

mockTransHistory :: Currency -> [TransactionMock]
mockTransHistory cur = [
  TransactionMock (moneyFromRational cur 0.63919646) "2020-04-07 14:12" TransRefill   (trMockInfo cur) TransUncofirmed
 ,TransactionMock (moneyFromRational cur 0.20134303) "2020-04-07 12:30" TransWithdraw (trMockInfo cur) TransConfirmed
 ,TransactionMock (moneyFromRational cur 0.40213010) "2020-04-03 10:30" TransRefill   (trMockInfo cur) TransConfirmed
 ,TransactionMock (moneyFromRational cur 0.02142302) "2020-03-20 22:40" TransWithdraw (trMockInfo cur) TransConfirmed
 ,TransactionMock (moneyFromRational cur 0.10024245) "2020-03-05 09:05" TransRefill   (trMockInfo cur) TransConfirmed]

trMockInfo :: Currency -> TransactionInfo
trMockInfo cur = TransactionInfo
  "330ce5f20e63b97604fb6add4e4be53197363ac5ebf7342e1372212b7b49498e"
  (Just "s3")
  "https://www.blockchain.com/btc/tx/330ce5f20e63b97604fb6add4e4be53197363ac5ebf7342e1372212b7b49498e"
  (moneyFromRational cur 0.000706)
  11
  "00000000000000000005119aeee8d2550c5875ff0569583d0ca543ed0c06b2d4"
  "010000000001033a049aac743259959d6202adbd69e533d0107f83ca21f278e2514ed3b160a80f03000000171600144ba622ca5a3babce0198b4b575ae5e23fbafa04affffffff6d5734b3d1289aaad6a6edce643c0d8c1d32408e4122c7915c4c979a100a377b0700000017160014d0050eb0fa0afa74876883971d194843aef8c3b8fffffffff7d510428b37b663a744b4e37d0c16dfaaffe2386dbb5fb5c77b6780acff4e8e0100000017160014d0050eb0fa0afa74876883971d194843aef8c3b8ffffffff024ffb49000000000017a914da2e37a0ac8f61fc60833fe4eb82f619992dc42887cf5a8503000000001976a9144eda7a74a3712d81fb0167d97e1119d673edf87b88ac02483045022100c687cb59f9d49e2b086a7fc17074f87612be3b7865e63add8f709384db8b34eb022016789c26d323c57acd590a6ad6db959d89cc9fbb8478b396e551666316264619012102f2246a7f2dd810498aa4f18fe86a6aef1e7856634d18e785b1eeda32712df96f02473044022004c6f0e07ee78f1bee6d20991ca1d237a139463d9612ca3df752296b3f1576c102205fd8d26e4c911aed931e0c0f8c006c0a94b13161d9a815a4809b525d3ae3754b012102e20a411a55d4e399dda618529c95b5a4fcdbd861dba466b38aa04e5b8f7324880247304402206ed672ce4e4b5db99461375afffcb393a29c8a5a959f21a3eb5a97b1bd4e4c3902204bab74dfca7b97f965c37ff34a6462aa391609f3793c561db27e141b312610f7012102e20a411a55d4e399dda618529c95b5a4fcdbd861dba466b38aa04e5b8f73248800000000"
  [("3MaebbZnWMXoxTWR7SHVGS3W6Xuw5FU164",(moneyFromRational cur 0.04848463),TOUnspent),("18BwS73Fq7D5HY8rGkYCsWNGXXRfEvDxW2",(moneyFromRational cur 0.59071183),TOSpent)]
  [("3Mx9XH35FrbpVjsDayKyvc6eSDZfjJAsx5",(moneyFromRational cur 0.13282286)),("3EVkfRx1cPWC8czue1RH4d6rTXghWyCXDS",(moneyFromRational cur 0.25622085)),("3EVkfRx1cPWC8czue1RH4d6rTXghWyCXDS",(moneyFromRational cur 0.25085875))]

data TransactionMock = TransactionMock {
  txAmount :: Money
 ,txDate   :: Text
 ,txInOut  :: TransType
 ,txInfo   :: TransactionInfo
 ,txStatus :: TransStatus
} deriving (Show)

data TransactionInfo = TransactionInfo {
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
