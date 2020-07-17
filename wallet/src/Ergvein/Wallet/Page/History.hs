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
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Alert
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
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.TimeZone
import Ergvein.Wallet.Tx

import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Worker.Node
import Ergvein.Wallet.Wrapper

import Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.List as L
import Network.Haskoin.Address
import Data.Maybe (fromMaybe, isJust)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Text as T
import Data.Serialize
import Data.Word
import Safe

import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Constants          as HK
import qualified Network.Haskoin.Script             as HK
import qualified Network.Haskoin.Transaction        as HK
import qualified Network.Haskoin.Util               as HK
{-
historyPageWeird :: MonadFront t m => Currency -> m ()
historyPageWeird cur = do
  title <- balanceTitleWidget cur
  let thisWidget = Just $ pure $ historyPage cur
      navbar = navbarWidget cur thisWidget NavbarHistory
  (txsD, hghtD) <- transactionsGetting BTC
  goE <- case cur of
    BTC -> do
      (txsD, hghtD) <- transactionsGetting BTC
      txClickDE <- widgetHoldDyn $ ffor txsD $ \txs ->
        if L.null txs
          then
            wrapperNavbar True title thisWidget navbar $ do
              localizedText HistoryNoTxs
              pure never
          else
            wrapperNavbar False title thisWidget navbar $ do
              hght <- sampleDyn hghtD
              txClickE <- traverse (historyTableRow hght) txs
              pure $ leftmost txClickE
      pure $ switchDyn txClickDE
    ERGO -> do
      txClickE <- traverse (historyTableRow 0) (mockTransHistory cur)
      pure $ leftmost txClickE
  void $ nextWidget $ ffor goE $ \tr -> Retractable {
      retractableNext = transactionInfoPage cur tr
    , retractablePrev = thisWidget
    } -}

historyPage :: MonadFront t m => Currency -> m ()
historyPage cur = do
  title <- balanceTitleWidget cur
  let thisWidget = Just $ pure $ historyPage cur
      navbar = navbarWidget cur thisWidget NavbarHistory
  navbarWidget cur thisWidget NavbarHistory
  goE <- historyTableWidget cur $ mockTransHistory cur
  void $ nextWidget $ ffor goE $ \tr -> Retractable {
      retractableNext = transactionInfoPage cur tr
    , retractablePrev = thisWidget
    }
{-
historyPage2 :: MonadFront t m => Currency -> m ()
historyPage2 cur = wrapper False (HistoryTitle cur) (Just $ pure $ historyPage cur) $ do
  ps <- getPubStorage
  pubSD <- getPubStorageD
  let thisWidget = Just $ pure $ historyPage cur
      historyWidget = historyTableWidgetD cur $ mockTransHistory cur
  navbarWidget cur thisWidget NavbarHistory
  goE <- historyWidget
  void $ nextWidget $ ffor goE $ \tr -> Retractable {
      retractableNext = transactionInfoPageD cur tr
    , retractablePrev = thisWidget
    }
-}

#ifdef ANDROID
transactionInfoPage :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPage cur tr@TransactionView{..} = wrapperTitle False HistoryTITitle (Just $ pure $ transactionInfoPage cur tr) $ divClass "tx-info-page" $ do
  (hashD, hashE, copiedHashE) <- divClass "tx-info-page-element" $ mdo
    hashD' <- expD hashE' hashD'
    hashE' <- expHead hashD' HistoryTIHash
    copiedHashE' <- copyDiv hashD' $ txId txInfoView
    pure (hashD', hashE', copiedHashE')
  case (txLabel txInfoView) of
    Just lbl -> divClass "tx-info-page-element" $ do
      par $ bold $ localizedText HistoryTILabel
      par $ text lbl
    Nothing -> pure ()
  divClass "tx-info-page-element" $ do
    let url = txUrl txInfoView
    par $ bold $ localizedText HistoryTIURL
    parClass "tx-info-page-expanded" $ hyperlink "link" url url
  divClass "tx-info-page-element" $ do
    par $ bold $ localizedText HistoryTIVolume
    par $ text $ showMoney txAmount <> " " <> showt cur
  divClass "tx-info-page-element" $ do
    par $ bold $ localizedText HistoryTIFee
    par $ text $ maybe "unknown" (\a -> (showMoney a) <> " " <> showt cur) $ txFee txInfoView
  divClass "tx-info-page-element" $ do
    par $ bold $ localizedText HistoryTIConfirmations
    par $ text $ showt $ txConfirmations txInfoView
  (blockD, blockE, copiedBlockE) <- divClass "tx-info-page-element" $ mdo
    blockD' <- expD blockE' blockD'
    blockE' <- expHead blockD' HistoryTIBlock
    copiedBlockE' <- copyDiv blockD' $ txBlock txInfoView
    pure (blockD', blockE', copiedBlockE')
  divClass "tx-info-page-element" $ do
    par $ bold $ localizedText HistoryTIOutputs
    divClass "tx-info-page-outputs-inputs" $ do
      flip traverse (txOutputs txInfoView) $ \(oAddress, oValue, oStatus) -> do
        divClass "pr-1" $ localizedText HistoryTIOutputsValue
        divClass "" $ text $ showMoney oValue <> " " <> showt cur
        divClass "pr-1" $ localizedText HistoryTIOutputsAddress
        divClass "tx-info-page-expanded" $ text $ oAddress
        divClass "mb-1 pr-1" $ localizedText HistoryTIOutputsStatus
        divClass "mb-1" $ localizedText oStatus
  divClass "tx-info-page-element" $ do
    par $ bold $ localizedText HistoryTIInputs
    divClass "tx-info-page-outputs-inputs" $ do
      flip traverse (txInputs txInfoView) $ \(oAddress, oValue) -> do
        divClass "pr-1" $ localizedText HistoryTIOutputsValue
        divClass "" $ text $ showMoney oValue <> " " <> showt cur
        divClass "pr-1 mb-1" $ localizedText HistoryTIOutputsAddress
        divClass "tx-info-page-expanded mb-1" $ text $ oAddress
  let copiedE = leftmost[
          (txId txInfoView) <$ copiedHashE
        , (txBlock txInfoView) <$ copiedBlockE
        ]
  cE <- clipboardCopy $ copiedE
  showSuccessMsg $ CSCopied <$ cE
  pure ()
  where
    expD expE expD' = holdDyn Minified $ poke expE $ \_ -> do
      st <- sampleDyn expD'
      case st of
        Minified -> pure Expanded
        Expanded -> pure Minified
    expPar statD txt = widgetHoldDyn $ ffor statD $ \exStatus -> case exStatus of
      Minified -> parClass "tx-info-page-minified" $ text txt
      Expanded -> parClass "tx-info-page-expanded" $ text txt
    expHead statD txt = divButton "tx-info-page-expand-buttton-wrapper" $ par $ bold $ do
      localizedText txt
      widgetHoldDyn $ ffor statD $ \exStatus -> case exStatus of
        Minified -> elClass "i" "tx-info-page-expand-buttton fas fa-caret-down" $ blank
        Expanded -> elClass "i" "tx-info-page-expand-buttton fas fa-caret-up" $ blank
    copyDiv copyD txt = divButton "tx-info-page-copy" $ do
      expPar copyD txt
#else
transactionInfoPage :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPage cur tr@TransactionView{..} = wrapperTitle False HistoryTITitle (Just $ pure $ transactionInfoPage cur tr) $ divClass "tx-info-page" $ do
  infoPageElementExp HistoryTIHash $ txId txInfoView
  case (txLabel txInfoView) of
    Just lbl -> infoPageElement HistoryTILabel lbl
    Nothing -> pure ()
  infoPageElementExpEl HistoryTIURL $ hyperlink "link" (txUrl txInfoView) (txUrl txInfoView)
  infoPageElement HistoryTIVolume $ showMoney txAmount <> " " <> showt cur
  infoPageElement HistoryTIFee $ maybe "unknown" (\a -> (showMoney a) <> " " <> showt cur) $ txFee txInfoView
  infoPageElement HistoryTIConfirmations $ showt $ txConfirmations txInfoView
  infoPageElementExp HistoryTIBlock $ txBlock txInfoView
  infoPageElementEl HistoryTIOutputs $ divClass "tx-info-page-outputs-inputs" $ do
      flip traverse (txOutputs txInfoView) $ \(oAddress, oValue, oStatus) -> do
        divClass "pr-1" $ localizedText HistoryTIOutputsValue
        divClass "" $ text $ showMoney oValue <> " " <> showt cur
        divClass "pr-1" $ localizedText HistoryTIOutputsAddress
        divClass "tx-info-page-expanded" $ text $ oAddress
        divClass "mb-1 pr-1" $ localizedText HistoryTIOutputsStatus
        divClass "mb-1" $ localizedText oStatus
      pure ()
  infoPageElementEl HistoryTIInputs $ divClass "tx-info-page-outputs-inputs" $ do
    flip traverse (txInputs txInfoView) $ \(oAddress, oValue) -> do
      divClass "pr-1" $ localizedText HistoryTIOutputsValue
      divClass "" $ text $ showMoney oValue <> " " <> showt cur
      divClass "pr-1 mb-1" $ localizedText HistoryTIOutputsAddress
      divClass "tx-info-page-expanded mb-1" $ text $ oAddress
    pure ()

transactionInfoPageD :: MonadFront t m => Currency -> Dynamic t TransactionView -> m ()
transactionInfoPageD cur trD = void $ widgetHoldDyn $ ffor trD $ \tr@TransactionView{..} -> wrapperTitle False HistoryTITitle (Just $ pure $ transactionInfoPage cur tr) $ divClass "tx-info-page" $ do
  infoPageElementExp HistoryTIHash $ txId txInfoView
  case (txLabel txInfoView) of
    Just lbl -> infoPageElement HistoryTILabel lbl
    Nothing -> pure ()
  infoPageElementExpEl HistoryTIURL $ hyperlink "link" (txUrl txInfoView) (txUrl txInfoView)
  infoPageElement HistoryTIVolume $ showMoney txAmount <> " " <> showt cur
  infoPageElement HistoryTIFee $ maybe "unknown" (\a -> (showMoney a) <> " " <> showt cur) $ txFee txInfoView
  infoPageElement HistoryTIConfirmations $ showt $ txConfirmations txInfoView
  infoPageElementExp HistoryTIBlock $ txBlock txInfoView
  infoPageElementEl HistoryTIOutputs $ divClass "tx-info-page-outputs-inputs" $ do
      flip traverse (txOutputs txInfoView) $ \(oAddress, oValue, oStatus) -> do
        divClass "pr-1" $ localizedText HistoryTIOutputsValue
        divClass "" $ text $ showMoney oValue <> " " <> showt cur
        divClass "pr-1" $ localizedText HistoryTIOutputsAddress
        divClass "tx-info-page-expanded" $ text $ oAddress
        divClass "mb-1 pr-1" $ localizedText HistoryTIOutputsStatus
        divClass "mb-1" $ localizedText oStatus
      pure ()
  infoPageElementEl HistoryTIInputs $ divClass "tx-info-page-outputs-inputs" $ do
    flip traverse (txInputs txInfoView) $ \(oAddress, oValue) -> do
      divClass "pr-1" $ localizedText HistoryTIOutputsValue
      divClass "" $ text $ showMoney oValue <> " " <> showt cur
      divClass "pr-1 mb-1" $ localizedText HistoryTIOutputsAddress
      divClass "tx-info-page-expanded mb-1" $ text $ oAddress
    pure ()
#endif

infoPageElement :: MonadFront t m => HistoryPageStrings -> Text -> m ()
infoPageElement hps txt = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  par $ text txt

infoPageElementEl :: MonadFront t m => HistoryPageStrings -> m () -> m ()
infoPageElementEl hps el = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  el

infoPageElementExp :: MonadFront t m => HistoryPageStrings -> Text -> m ()
infoPageElementExp hps txt = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  parClass "tx-info-page-expanded" $ text $ txt

infoPageElementExpEl :: MonadFront t m => HistoryPageStrings -> m () -> m ()
infoPageElementExpEl hps el = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  parClass "tx-info-page-expanded" $ el

historyTableWidgetOld :: MonadFront t m => Currency -> [TransactionView] -> m (Event t TransactionView)
historyTableWidgetOld cur trList = case cur of
  BTC -> do
    (txsD,hghtD) <- transactionsGetting BTC
    txClickDE <- widgetHoldDyn $ ffor txsD $ \txs -> do
      hght <- sampleDyn hghtD
      txClickE <- traverse historyTableRow txs
      pure $ leftmost txClickE
    pure $ switchDyn txClickDE
  ERGO -> do
    txClickE <- traverse historyTableRow trList
    pure $ leftmost txClickE

historyTableWidget :: MonadFront t m => Currency -> [TransactionView] -> m (Event t TransactionView)
historyTableWidget cur trList = case cur of
  BTC -> do
    (txsD,hghtD) <- transactionsGetting BTC
    let txMapD = Map.fromList . L.zip [0..] <$> txsD
    mapED <- listWithKey txMapD (\_ -> historyTableRowD1 hghtD)
    let txClickE = switchDyn $ mergeMap <$> mapED
    pure $ fmapMaybe id $ headMay . Map.elems <$> txClickE
  ERGO -> do
    txClickE <- traverse historyTableRow trList
    pure $ leftmost txClickE

historyTableWidgetD :: MonadFront t m => Currency -> [TransactionView] -> m (Event t (Dynamic t TransactionView))
historyTableWidgetD cur trList = case cur of
  _ -> do
    (txsD,hghtD) <- transactionsGetting BTC
    let dynTList  = fmap (L.zip [0..]) txsD
        dynTList2 = fmap (\z -> fmap (\(a,b) -> fmap (\h -> h !! a) txsD) z) dynTList
        --dyn2 = sequenceA txsD
    txDE <- widgetHoldDyn $ ffor dynTList2 $ \listTxD -> do
      txCliListE <- traverse historyTableRowD listTxD
      pure $ leftmost txCliListE
    pure $ switchDyn txDE
{-
historyTableWidgetD :: MonadFront t m => Currency -> [TransactionView] -> m (Event t (Dynamic t TransactionView))
historyTableWidgetD cur trList = case cur of
  _ -> do
    (txsD,hghtD) <- transactionsGetting BTC
    txDE <- widgetHoldDyn $ ffor txsD $ \listTxD -> do
      txCliListE <- fmap historyTableRowD listTxD
      pure $ leftmost txCliListE
    pure $ switchDyn txDE -}

historyTableRow :: MonadFront t m => TransactionView -> m (Event t TransactionView)
historyTableRow tr@TransactionView{..} = divButton "history-table-row" $ do
  divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ symb $ text $ showMoney txAmount
  divClass "history-date" $ text $ txDate
  divClass ("history-status-" <> ((T.toLower . showt) txInOut) <> " history-" <> confsClass) $ text $ showt confs
  pure tr
  where
    confs = txConfirmations txInfoView
    confsClass =
      if (confs == 0)
        then "unconfirmed"
      else if (confs > 0 && confs < confirmationGap)
        then "partially-confirmed"
      else "confirmed"
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

historyTableRowD1 :: MonadFront t m => Dynamic t Word64 -> Dynamic t TransactionView -> m (Event t TransactionView)
historyTableRowD1 _ trD = fmap switchDyn $ widgetHoldDyn $ ffor trD $ \tr@TransactionView{..} -> divButton "history-table-row" $ do
    divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ symb txInOut $ text $ showMoney txAmount
    divClass "history-date" $ text $ txDate
    divClass ("history-status-" <> ((T.toLower . showt) txInOut) <> " history-" <> (confsClass tr)) $ text $ showt $ confs tr
    pure tr
  where
    confs tr = txConfirmations $ txInfoView tr
    confsClass tr = if (confs tr == 0)
      then "unconfirmed"
      else "confirmed"
    symb :: MonadFront t m => TransType -> m a -> m a
    symb trInOut ma = case trInOut of
      TransRefill -> do
        spanClass "history-page-sign-icon" $ elClass "i" "fas fa-plus fa-fw" blank
        ma
      TransWithdraw -> do
        spanClass "history-page-sign-icon" $ elClass "i" "fas fa-minus fa-fw" blank
        ma

historyTableRowD :: MonadFront t m => Dynamic t TransactionView -> m (Event t (Dynamic t TransactionView))
historyTableRowD trD = divButton "history-table-row" $ do
  tD <- widgetHoldDyn $ ffor trD $ \tr@TransactionView{..} -> do
    divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ symb txInOut $ text $ showMoney txAmount
    divClass "history-date" $ text $ txDate
    divClass ("history-status-" <> ((T.toLower . showt) txInOut) <> " history-" <> (confsClass tr)) $ text $ showt $ confs tr
  pure trD
  where
    confs tr = txConfirmations $ txInfoView tr
    confsClass tr = if (confs tr == 0)
      then "unconfirmed"
      else "confirmed"
    symb :: MonadFront t m => TransType -> m a -> m a
    symb trInOut ma = case trInOut of
      TransRefill -> do
        spanClass "history-page-sign-icon" $ elClass "i" "fas fa-plus fa-fw" blank
        ma
      TransWithdraw -> do
        spanClass "history-page-sign-icon" $ elClass "i" "fas fa-minus fa-fw" blank
        ma

-- | Extract addresses from keystore
extractAddrs :: PubKeystore -> [(Maybe Int, EgvAddress)]
extractAddrs (PubKeystore mast ext int) = mastadr:(extadrs <> intadrs)
  where
    mastadr = (Nothing,) $ egvXPubKeyToEgvAddress mast
    extadrs = V.toList $ V.imap (\i b -> (Just i, egvXPubKeyToEgvAddress $ pubKeyBox'key b)) ext
    intadrs = V.toList $ V.imap (\i b -> (Nothing, egvXPubKeyToEgvAddress $ pubKeyBox'key b)) int

transactionsGetting :: MonadFront t m => Currency -> m (Dynamic t [TransactionView],Dynamic t Word64)
transactionsGetting cur = do
  buildE <- delay 0.2 =<< getPostBuild
  ps <- getPubStorage
  pubSD <- getPubStorageD
  let mStor psBS = fromMaybe 0 $ maybe (Just 0) _currencyPubStorage'height $ Map.lookup cur (_pubStorage'currencyPubStorages psBS)
  heightD <- holdDyn (mStor ps) $ poke (updated pubSD) $ \pbs -> pure $ mStor pbs
  let allBtcAddrsD = ffor pubSD $ \(PubStorage _ cm _ _) -> case Map.lookup BTC cm of
        Nothing -> []
        Just CurrencyPubStorage{..} -> extractAddrs _currencyPubStorage'pubKeystore

  abS <- filtArd <$> sampleDyn allBtcAddrsD
  tzE <- getGetTimeZone buildE

  tzD <- holdDyn utc tzE

  let rawTxList = filterTx abS ps

  hD <- holdDyn rawTxList $ poke (updated pubSD) $ \pbs -> do
    allbtcAdrS <- filtArd <$> sampleDyn allBtcAddrsD
    pure $ filterTx allbtcAdrS pbs

  filtrTxListSE <- performFork $ ffor tzE $ \_ -> do
    let tx = filterTx abS ps
    tz <- sampleDyn tzD
    ps' <- sampleDyn pubSD
    getAndFilterBlocks heightD allBtcAddrsD tz tx ps'

  filtrTxListE <- performFork $ ffor (updated hD) $ \tx -> do
    tz <- sampleDyn tzD
    ps' <- sampleDyn pubSD
    getAndFilterBlocks heightD allBtcAddrsD tz tx ps'

  sD <- holdDyn [] filtrTxListSE
  hS <- sampleDyn $ sD

  filtrHD <- holdDyn [] $ leftmost [filtrTxListSE, filtrTxListE]
  pure (filtrHD, heightD)
  where
    getAndFilterBlocks heightD btcAddrsD tz tx store = do
      allbtcAdrS <- filtArd <$> sampleDyn btcAddrsD
      hght <- sampleDyn heightD
      liftIO $ flip runReaderT store $ do
        blh <- traverse getBtcBlockHashByTxHash $ fmap HK.txHash $ fmap (getBtcTx) tx
        bl <- traverse (maybe (pure Nothing) getBlockHeaderByHash) blh
        b <- traverse (checkAddr allbtcAdrS) tx
        let txRefList = fmap (calcRefill (fmap getBtcAddr allbtcAdrS)) tx
        pure $ L.reverse $ L.sortOn (\tx -> txDate tx) $ fmap snd $ L.filter fst $ L.zip b (prepareTransactionView hght tz <$> txListRaw bl blh tx txRefList)

    filterTx ac pubS = case cur of
      BTC  -> fmap snd $ fromMaybe [] $ fmap Map.toList $ _currencyPubStorage'transactions <$> Map.lookup cur (_pubStorage'currencyPubStorages pubS)
      ERGO -> []

    calcRefill ac tx = case tx of
        BtcTx btx _ -> Money cur $ sum $ fmap (HK.outValue . snd) $ L.filter (maybe False (flip elem ac . fromSegWit) . fst) $ fmap (\txo -> (getSegWitAddr txo,txo)) $ HK.txOut btx
        ErgTx etx _ -> Money cur 0

    checkAddr :: (HasPubStorage m, PlatformNatives) => [EgvAddress] -> EgvTx -> m Bool
    checkAddr ac tx = do
      bL <- traverse (flip checkAddrTx (getBtcTx tx)) ac
      pure $ L.or bL

    filtArd :: [(Maybe Int, EgvAddress)] -> [EgvAddress]
    filtArd madr = fmap snd $ L.filter (isJust . fst) madr

    txListRaw (a:as) (b:bs) (c:cs) (d:ds) = (TxRawInfo a b c d) : txListRaw as bs cs ds

prepareTransactionView :: Word64 -> TimeZone -> TxRawInfo -> TransactionView
prepareTransactionView hght tz TxRawInfo{..} = TransactionView {
    txAmount = txom
  , txDate = blTime
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
     ,txFee           = Nothing
     ,txConfirmations = bHeight
     ,txBlock         = txBlockDebug
     ,txOutputs       = txOuts
     ,txInputs        = []
    }
    btx = getBtcTx txr
    blHght = maybe 0 etxMetaHeight $ getBtcTxMeta txr
    bHeight = if ((blHght == 0) || (hght == 0))
      then 0
      else hght - blHght + 1
    txHs = HK.txHash btx
    txHex = HK.txHashToHex txHs
    txOuts = fmap (\out -> (txOutAdr out,Money BTC (HK.outValue out), TOUnspent)) $ HK.txOut btx
    txIns = fmap (\out -> (txOutAdr out,Money BTC (HK.outValue out), TOUnspent)) $ HK.txOut btx
    txOutAdr out = maybe "undefined" (fromMaybe "unknown" . (addrToString HK.btcTest) . fromSegWit) $ getSegWitAddr out
    txBlockDebug = maybe "unknown" HK.blockHashToHex txHBl
    blTime = maybe "pending.." (T.pack . secToTimestamp . HK.blockTimestamp) txMBl

    secToTimestamp t = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" $ utcToZonedTime tz $ posixSecondsToUTCTime $ fromIntegral t

-- Front types, should be moved to Utils
data ExpStatus = Expanded | Minified deriving (Eq, Show)

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
  (Just $ moneyFromRational cur 0.000706)
  11
  "00000000000000000005119aeee8d2550c5875ff0569583d0ca543ed0c06b2d4"
  [("3MaebbZnWMXoxTWR7SHVGS3W6Xuw5FU164",(moneyFromRational cur 0.04848463),TOUnspent),("18BwS73Fq7D5HY8rGkYCsWNGXXRfEvDxW2",(moneyFromRational cur 0.59071183),TOSpent)]
  [("3Mx9XH35FrbpVjsDayKyvc6eSDZfjJAsx5",(moneyFromRational cur 0.13282286)),("3EVkfRx1cPWC8czue1RH4d6rTXghWyCXDS",(moneyFromRational cur 0.25622085)),("3EVkfRx1cPWC8czue1RH4d6rTXghWyCXDS",(moneyFromRational cur 0.25085875))]

data TxRawInfo = TxRawInfo {
  txMBl :: Maybe HK.BlockHeader
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
 ,txFee           :: Maybe Money
 ,txConfirmations :: Word64
 ,txBlock         :: Text
 ,txOutputs       :: [(Text,Money,TransOutputType)]
 ,txInputs        :: [(Text,Money)]
} deriving (Show)
