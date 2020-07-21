{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Transaction(
    transactionInfoPage
  , transactionsGetting
  , showTime
  , symb
  , stat
  , TransactionView(..)
  , TransactionViewInfo(..)
  , TxRawInfo(..)
  , TxTime(..)
  , ExpStatus(..)
  , TransStatus(..)
  , TransType(..)
  , TransOutputType(..)
  ) where

import Control.Lens.Combinators
import Control.Monad.Reader

import Ergvein.Filters.Btc
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
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
import qualified Data.List as L
import Network.Haskoin.Address
import Data.Maybe (fromMaybe, isJust, catMaybes)
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

#ifdef ANDROID
transactionInfoPage :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPage cur tr@TransactionView{..} = do
  title <- localized HistoryTITitle
  wrapper False title (Just $ pure $ transactionInfoPage cur tr) $ divClass "tx-info-page" $ do
    (hashD, hashE, copiedHashE) <- divClass "tx-info-page-element" $ mdo
      hashD' <- expD hashE' hashD'
      hashE' <- expHead hashD' HistoryTIHash
      copiedHashE' <- copyDiv hashD' $ txId txInfoView
      pure (hashD', hashE', copiedHashE')
    case (txLabel txInfoView) of
        Just lbl -> infoPageElement HistoryTILabel lbl
        Nothing -> pure ()
    infoPageElementExpEl HistoryTIURL $ hyperlink "link" (txUrl txInfoView) (txUrl txInfoView)
    infoPageElement HistoryTIAmount $ (symb txInOut) $ showMoney txAmount <> " " <> showt cur
    infoPageElement HistoryTIFee $ maybe "unknown" (\a -> (showMoney a) <> " " <> showt cur) $ txFee txInfoView
    infoPageElement HistoryTIConfirmations $ showt $ txConfirmations txInfoView
    (blockD, blockE, copiedBlockE) <- divClass "tx-info-page-element" $ mdo
      blockD' <- expD blockE' blockD'
      blockE' <- expHead blockD' HistoryTIBlock
      copiedBlockE' <- case txBlock txInfoView of
        Nothing -> copyDivLoc blockD' HistoryTIBlockUndefined
        Just blockHash -> copyDiv blockD' blockHash
      pure (blockD', blockE', copiedBlockE')
    infoPageElementEl HistoryTIOutputs $ divClass "tx-info-page-outputs-inputs" $ do
        flip traverse (txOutputs txInfoView) $ \(oAddress, oValue, oStatus, isOur) -> do
          divClass "pr-1" $ localizedText HistoryTIOutputsValue
          divClass "" $ text $ showMoney oValue <> " " <> showt cur
          divClass "pr-1" $ localizedText HistoryTIOutputsAddress
          divClass "tx-info-page-expanded" $ case oAddress of
            Nothing -> localizedText HistoryTIAddressUndefined
            Just address -> text $ address
          divClass "mb-1 pr-1" $ localizedText HistoryTIOutputsStatus
          divClass "mb-1" $ localizedText oStatus
        pure ()
    infoPageElementEl HistoryTIInputs $ divClass "tx-info-page-outputs-inputs" $ do
        flip traverse (txInputs txInfoView) $ \(oAddress, inValue) -> do
          divClass "pr-1" $ localizedText HistoryTIOutputsValue
          divClass "" $ text $ showMoney inValue <> " " <> showt cur
          divClass "pr-1 mb-1" $ localizedText HistoryTIOutputsAddress
          divClass "tx-info-page-expanded mb-1" $ text $ showt oAddress
        pure ()
    let copiedE = leftmost[
            (txId txInfoView) <$ copiedHashE
          , (maybe "" id $ txBlock txInfoView) <$ copiedBlockE
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
      expParLoc statD ltxt = widgetHoldDyn $ ffor statD $ \exStatus -> case exStatus of
        Minified -> parClass "tx-info-page-minified" $ localizedText ltxt
        Expanded -> parClass "tx-info-page-expanded" $ localizedText ltxt
      expHead statD txt = divButton "tx-info-page-expand-buttton-wrapper" $ par $ bold $ do
        localizedText txt
        widgetHoldDyn $ ffor statD $ \exStatus -> case exStatus of
          Minified -> elClass "i" "tx-info-page-expand-buttton fas fa-caret-down" $ blank
          Expanded -> elClass "i" "tx-info-page-expand-buttton fas fa-caret-up" $ blank
      copyDiv copyD txt = divButton "tx-info-page-copy" $ do
        expPar copyD txt
      copyDivLoc copyD ltxt = divButton "tx-info-page-copy" $ do
        expParLoc copyD ltxt
#else
transactionInfoPageOld :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPageOld cur tr@TransactionView{..} = do
  title <- localized HistoryTITitle
  wrapper False title (Just $ pure $ transactionInfoPage cur tr) $ divClass "tx-info-page" $ do
    infoPageElementExp HistoryTIHash $ txId txInfoView
    case (txLabel txInfoView) of
      Just lbl -> infoPageElement HistoryTILabel lbl
      Nothing -> pure ()
    infoPageElementExpEl HistoryTIURL $ hyperlink "link" (txUrl txInfoView) (txUrl txInfoView)
    infoPageElement HistoryTIAmount $ showMoney txAmount <> " " <> showt cur
    infoPageElement HistoryTIFee $ maybe "unknown" (\a -> (showMoney a) <> " " <> showt cur) $ txFee txInfoView
    infoPageElement HistoryTIConfirmations $ showt $ txConfirmations txInfoView
    infoPageElementExp HistoryTIBlock $ maybe "unknown" id $ txBlock txInfoView
    infoPageElementEl HistoryTIOutputs $ divClass "tx-info-page-outputs-inputs" $ do
        flip traverse (txOutputs txInfoView) $ \(oAddress, oValue, oStatus, isOur) -> do
          divClass "pr-1" $ localizedText HistoryTIOutputsValue
          divClass "" $ text $ showMoney oValue <> " " <> showt cur
          divClass "pr-1" $ localizedText HistoryTIOutputsAddress
          divClass "tx-info-page-expanded" $ case oAddress of
            Nothing -> localizedText HistoryTIAddressUndefined
            Just address -> text address
          divClass "mb-1 pr-1" $ localizedText HistoryTIOutputsStatus
          divClass "mb-1" $ localizedText oStatus
        pure ()
    infoPageElementEl HistoryTIInputs $ divClass "tx-info-page-outputs-inputs" $ do
      flip traverse (txInputs txInfoView) $ \(oAddress, oValue) -> do
        divClass "pr-1" $ localizedText HistoryTIOutputsValue
        divClass "" $ text $ showMoney oValue <> " " <> showt cur
        divClass "pr-1 mb-1" $ localizedText HistoryTIOutputsAddress
        divClass "tx-info-page-expanded mb-1" $ text $ showt oAddress
      pure ()

transactionInfoPage :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPage cur tr@TransactionView{..} = do
  title <- localized HistoryTITitle
  wrapper False title (Just $ pure $ transactionInfoPage cur tr) $ divClass "tx-info-page" $ do
    infoPageElementExpEl HistoryTITransactionId $ hyperlink "link" (txId txInfoView) (txUrl txInfoView)
    infoPageElementEl HistoryTIAmount $ (symbCol txInOut) $ text $ showMoney txAmount <> " " <> showt cur
    case txInOut of
      TransRefill -> pure ()
      TransWithdraw -> infoPageElement HistoryTIFee $ maybe "unknown" (\a -> (showMoney a) <> " " <> showt cur) $ txFee txInfoView
    infoPageElement HistoryTIConfirmations $ showt $ txConfirmations txInfoView
    infoPageElementExp HistoryTIBlock $ maybe "unknown" id $ txBlock txInfoView
    case txInOut of
      TransRefill -> pure ()
      TransWithdraw -> infoPageElementEl HistoryTIInputs $ divClass "tx-info-page-outputs-inputs" $ do
        flip traverse (txInputs txInfoView) $ \(oAddress, oValue) -> do
          divClass "pr-1" $ localizedText HistoryTIOutputsValue
          divClass "" $ text $ showMoney oValue <> " " <> showt cur
          divClass "pr-1 mb-1" $ localizedText HistoryTIOutputsAddress
          divClass "tx-info-page-expanded mb-1" $ text $ showt oAddress
        pure ()
    infoPageElementEl HistoryTIOutputs $ divClass "tx-info-page-outputs-inputs" $ do
      flip traverse (txOutputs txInfoView) $ \(oAddress, oValue, oStatus, isOur) -> do
        divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsValue
        divClass (oBld "" isOur) $ text $ showMoney oValue <> " " <> showt cur
        if isOur
          then divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsOurAddress
          else divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsAddress
        divClass (oBld "tx-info-page-expanded" isOur) $ case oAddress of
          Nothing -> localizedText HistoryTIAddressUndefined
          Just address -> text address
        divClass (oBld "mb-1 pr-1" isOur) $ localizedText HistoryTIOutputsStatus
        divClass (oBld "mb-1" isOur) $ localizedText oStatus
      pure ()
      where
        oBld txt isOur = if isOur then (txt <> " tx-info-our-address") else txt

#endif

showTime :: MonadFront t m => TransactionView -> m ()
showTime tr@TransactionView{..} = case txDate of
  TxTime Nothing -> do
    localizedText $ if txStatus == TransUncofirmedParents then HistoryUnconfirmedParents else HistoryUnconfirmed
  TxTime (Just date) -> do
    text $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S %d/%m/%Y" $ date

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

symb :: MonadFront t m => TransType -> m a -> m a
symb txInOut ma = case txInOut of
  TransRefill -> do
    spanClass "history-page-sign-icon" $ elClass "i" "fas fa-plus fa-fw" blank
    ma
  TransWithdraw -> do
    spanClass "history-page-sign-icon" $ elClass "i" "fas fa-minus fa-fw" blank
    ma

symbCol :: MonadFront t m => TransType -> m a -> m a
symbCol txInOut ma = divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ do
  case txInOut of
    TransRefill -> do
      spanClass "history-page-sign-icon" $ elClass "i" "fas fa-plus fa-fw" blank
      ma
    TransWithdraw -> do
      spanClass "history-page-sign-icon" $ elClass "i" "fas fa-minus fa-fw" blank
      ma

stat :: MonadFront t m => TransStatus -> m ()
stat txStatus = case txStatus of
  TransConfirmed -> spanClass "history-page-status-icon" $ elClass "i" "fas fa-check fa-fw" $ blank
  TransUncofirmed -> spanClass "history-page-status-icon" $ elClass "i" "fas fa-question fa-fw" $ blank

transactionsGetting :: MonadFront t m => Currency -> m (Dynamic t [TransactionView], Dynamic t Word64)
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
    let txs = filterTx abS ps
    tz <- sampleDyn tzD
    ps' <- sampleDyn pubSD
    getAndFilterBlocks heightD allBtcAddrsD tz txs ps'

  filtrTxListE <- performFork $ ffor (updated hD) $ \txs -> do
    tz <- sampleDyn tzD
    ps' <- sampleDyn pubSD
    getAndFilterBlocks heightD allBtcAddrsD tz txs ps'

  sD <- holdDyn [] filtrTxListSE
  hS <- sampleDyn $ sD

  filtrHD <- holdDyn [] $ leftmost [filtrTxListSE, filtrTxListE]
  pure (filtrHD, heightD)
  where
    getAndFilterBlocks heightD btcAddrsD tz txs store = do
      allbtcAdrS <- filtArd <$> sampleDyn btcAddrsD
      hght <- sampleDyn heightD
      liftIO $ flip runReaderT store $ do
        let txHashes = fmap (HK.txHash . getBtcTx) txs
            txsRefList = fmap (calcRefill (fmap getBtcAddr allbtcAdrS)) txs
            parentTxsIds = (fmap . fmap) (HK.txHashToHex . HK.outPointHash . HK.prevOutput) (fmap (HK.txIn . getBtcTx) txs)
        blh <- traverse getBtcBlockHashByTxHash txHashes
        bl <- traverse (maybe (pure Nothing) getBlockHeaderByHash) blh
        txStore <- getTxStorage cur
        flip runReaderT txStore $ do
          bInOut <- traverse (checkAddrInOut allbtcAdrS) txs
          parentTxs <- sequenceA $ fmap (traverse getTxById) parentTxsIds
          let getTxConfirmations mTx = case mTx of
                Nothing -> 1 -- If tx is not found we put 1 just to indicate that the transaction is confirmed
                Just tx -> maybe 0 (\x -> hght - x + 1) (fmap etxMetaHeight $ getBtcTxMeta tx)
              txParentsConfirmations = (fmap . fmap) getTxConfirmations parentTxs
              hasUnconfirmedParents = fmap (L.any (== 0)) txParentsConfirmations
          let rawTxs  = txListRaw bl blh txs txsRefList hasUnconfirmedParents
              rawTxsL = L.filter (\(a,b) -> a/=Nothing) $ L.zip bInOut $ txListRaw bl blh txs txsRefList hasUnconfirmedParents parentTxs
          pure $ L.reverse $ L.sortOn txDate $ (prepareTransactionView allbtcAdrS hght tz <$> rawTxsL)

    filterTx ac pubS = case cur of
      BTC  -> fmap snd $ fromMaybe [] $ fmap Map.toList $ _currencyPubStorage'transactions <$> Map.lookup cur (_pubStorage'currencyPubStorages pubS)
      ERGO -> []

    calcRefill ac tx = case tx of
        BtcTx btx _ -> Money cur $ sum $ fmap (HK.outValue . snd) $ L.filter (maybe False (flip elem ac . fromSegWit) . fst) $ fmap (\txo -> (getSegWitAddr txo,txo)) $ HK.txOut btx
        ErgTx etx _ -> Money cur 0

    checkAddr :: (HasTxStorage m, PlatformNatives) => [EgvAddress] -> EgvTx -> m Bool
    checkAddr ac tx = do
      bL <- traverse (flip checkAddrTx (getBtcTx tx)) ac
      pure $ L.or bL

    checkAddrInOut :: (HasTxStorage m, PlatformNatives) => [EgvAddress] -> EgvTx -> m (Maybe TransType)
    checkAddrInOut ac tx = do
      bLIn <- traverse (flip checkAddrTxIn (getBtcTx tx)) ac
      bLOut <- traverse (flip checkAddrTxOut (getBtcTx tx)) ac
      let isIn = L.or bLIn
          isOut = L.or bLOut
      if (not (isIn || isOut))
        then pure Nothing
        else if isIn
          then pure $ Just TransWithdraw
          else pure $ Just TransRefill

    filtArd :: [(Maybe Int, EgvAddress)] -> [EgvAddress]
    filtArd madr = fmap snd $ L.filter (isJust . fst) madr

    txListRaw (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) = (TxRawInfo a b c d e f) : txListRaw as bs cs ds es fs

prepareTransactionView :: [EgvAddress] -> Word64 -> TimeZone -> (Maybe TransType, TxRawInfo) -> TransactionView
prepareTransactionView addrs hght tz (mTT, TxRawInfo{..}) = TransactionView {
    txAmount = txom
  , txDate = blockTime
  , txInOut = fromMaybe TransRefill mTT
  , txInfoView = txInf
  , txStatus =
      if txHasUnconfirmedParents
        then TransUncofirmedParents
      else if (bHeight == 0)
        then TransUncofirmed
      else TransConfirmed
  }
  where
    txInf = TransactionViewInfo {
      txId            = txHex
     ,txLabel         = Nothing
     ,txUrl           = if isTestnet
       then "https://www.blockchain.com/btc-testnet/tx/" <> txHex
       else "https://www.blockchain.com/btc/tx/" <> txHex
     ,txFee           = txFeeCalc
     ,txConfirmations = bHeight
     ,txBlock         = txBlockDebug
     ,txOutputs       = txOuts
     ,txInputs        = txInsOuts
    }
    btx = getBtcTx txr
    blHght = maybe 0 etxMetaHeight $ getBtcTxMeta txr
    bHeight = if ((blHght == 0) || (hght == 0))
      then 0
      else hght - blHght + 1
    txHs = HK.txHash btx
    txHex = HK.txHashToHex txHs
    txOuts = fmap (\out -> (txOutAdr out, Money BTC (HK.outValue out), TOUnspent, txOurArdCheck out)) $ HK.txOut btx
    txOutsAm = fmap (\out -> HK.outValue out) $ HK.txOut btx
    txInsOuts = fmap fst $ L.filter snd $ fmap (\out -> ((txOutAdr out, Money BTC (HK.outValue out)), txOurArdCheck out)) $ L.concat $ fmap (HK.txOut . getBtcTx) $ catMaybes txParents
    txInsOutsAm = fmap fst $ L.filter snd $ fmap (\out -> (HK.outValue out,txOurArdCheck out)) $ L.concat $ fmap (HK.txOut . getBtcTx) $ catMaybes txParents
    network = getBtcNetwork $ getCurrencyNetwork BTC
    txOutAdr out = either (const Nothing) id $ (addrToString network) <$> (scriptToAddressBS $ HK.scriptOutput out)
    txOurArdCheck out = either (\_ -> False) (\a -> a `L.elem` btcAddrs) $ (scriptToAddressBS $ HK.scriptOutput out)
    txBlockDebug = maybe Nothing (Just . HK.blockHashToHex) txHBl
    blockTime = TxTime $ maybe Nothing (Just . secToTimestamp . HK.blockTimestamp) txMBl
    secToTimestamp t = utcToZonedTime tz $ posixSecondsToUTCTime $ fromIntegral t
    btcAddrs = fmap getBtcAddr addrs
    txFeeCalc = case mTT of
      Nothing -> Nothing
      Just TransRefill -> Nothing
      Just TransWithdraw -> Just $ Money BTC $ (sum txInsOutsAm) - (sum txOutsAm)
    txAmountCalc = 0

-- Front types, should be moved to Utils
data ExpStatus = Expanded | Minified deriving (Eq, Show)

data TransStatus = TransUncofirmed | TransUncofirmedParents | TransConfirmed deriving (Eq,Show)

data TransType = TransRefill | TransWithdraw deriving (Eq,Show)
data TransOutputType = TOSpent | TOUnspent deriving (Eq,Show)

instance LocalizedPrint TransOutputType where
  localizedShow l v = case l of
    English -> case v of
      TOSpent   -> "Spent"
      TOUnspent -> "Unspent"
    Russian -> case v of
      TOSpent   -> "Потрачен"
      TOUnspent -> "Не потрачен"


data TxRawInfo = TxRawInfo {
    txMBl                   :: Maybe HK.BlockHeader
  , txHBl                   :: Maybe HK.BlockHash
  , txr                     :: EgvTx
  , txom                    :: Money
  , txHasUnconfirmedParents :: Bool
  , txParents                :: [Maybe EgvTx]
} deriving (Show)

newtype TxTime = TxTime (Maybe ZonedTime) deriving (Show)

instance Eq TxTime where
  TxTime Nothing == TxTime Nothing = True
  TxTime (Just x) == TxTime (Just y) = zonedTimeToUTC x == zonedTimeToUTC y
  _ == _ = False

instance Ord TxTime where
  compare (TxTime Nothing) (TxTime Nothing) = EQ
  compare (TxTime Nothing) _ = GT
  compare _ (TxTime Nothing) = LT
  compare (TxTime (Just x)) (TxTime (Just y)) = compare (zonedTimeToUTC x) (zonedTimeToUTC y)

data TransactionView = TransactionView {
    txAmount   :: Money
  , txDate     :: TxTime
  , txInOut    :: TransType
  , txInfoView :: TransactionViewInfo
  , txStatus   :: TransStatus
} deriving (Show)

data TransactionViewInfo = TransactionViewInfo {
    txId            :: Text
  , txLabel         :: Maybe Text
  , txUrl           :: Text
  , txFee           :: Maybe Money
  , txConfirmations :: Word64
  , txBlock         :: Maybe Text
  , txOutputs       :: [(Maybe Text, Money, TransOutputType, Bool)]
  , txInputs        :: [(Maybe Text, Money)]
} deriving (Show)
