{-# LANGUAGE OverloadedLists #-}
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
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.TimeZone
import Ergvein.Wallet.Tx

import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Worker.Node
import Ergvein.Wallet.Wrapper

import Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector as V
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

transactionInfoPage :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPage cur tr@TransactionView{..} = do
  title <- localized HistoryTITitle
  moneyUnits <- fmap (fromMaybe defUnits . settingsUnits) getSettings
  wrapper False title (Just $ pure $ transactionInfoPage cur tr) $ divClass "tx-info-page" $ do
    infoPageElementExpEl HistoryTITransactionId $ hyperlink "link" (txId txInfoView) (txUrl txInfoView)
    infoPageElementEl HistoryTIAmount $ (symbCol txInOut) $ text $ showMoneyUnit txAmount moneyUnits <> " " <> symbolUnit cur moneyUnits
    infoPageElementEl HistoryTIWalletChanges $ (transTypeCol txInOut) $ text $ case txInOut of
      TransRefill -> (showMoneyUnit (Money BTC (maybe 0 moneyAmount txPrevAm)) moneyUnits) <> " -> " <> (showMoneyUnit (Money BTC ((maybe 0 moneyAmount txPrevAm) + (moneyAmount txAmount))) moneyUnits) <> " " <> symbolUnit cur moneyUnits
      TransWithdraw -> (showMoneyUnit (Money BTC (maybe 0 moneyAmount txPrevAm)) moneyUnits) <> " -> " <> (showMoneyUnit (Money BTC ((maybe 0 moneyAmount txPrevAm) - (moneyAmount txAmount) - (maybe 0 moneyAmount (txFee txInfoView)))) moneyUnits) <> " " <> symbolUnit cur moneyUnits
    case txInOut of
      TransRefill -> pure ()
      TransWithdraw -> infoPageElement HistoryTIFee $ maybe "unknown" (\a -> (showMoneyUnit a moneyUnits) <> " " <> symbolUnit cur moneyUnits) $ txFee txInfoView
    infoPageElementEl HistoryTITime $ showTime tr
    infoPageElement HistoryTIConfirmations $ showt $ txConfirmations txInfoView
    infoPageElementExpEl HistoryTIBlock $ maybe (text "unknown") (\(bllink,bl) -> hyperlink "link" bl bllink) $ txBlock txInfoView
    case txInOut of
      TransRefill -> pure ()
      TransWithdraw -> infoPageElementEl HistoryTIInputs $ divClass "tx-info-page-outputs-inputs" $ do
        flip traverse (txInputs txInfoView) $ \(oAddress, oValue) -> do
          divClass "pr-1" $ localizedText HistoryTIOutputsValue
          divClass "" $ text $ showMoneyUnit oValue moneyUnits <> " " <> symbolUnit cur moneyUnits
          divClass "pr-1 mb-1" $ localizedText HistoryTIOutputsAddress
          divClass "tx-info-page-expanded mb-1" $ case oAddress of
            Nothing -> localizedText HistoryTIAddressUndefined
            Just address -> text address
        pure ()
    infoPageElementEl HistoryTIOutputs $ divClass "tx-info-page-outputs-inputs" $ do
      flip traverse (txOutputs txInfoView) $ \(oAddress, oValue, oStatus, isOur) -> do
        divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsValue
        divClass (oBld "" isOur) $ text $ showMoneyUnit oValue moneyUnits <> " " <> symbolUnit cur moneyUnits
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

transTypeCol :: MonadFront t m => TransType -> m a -> m a
transTypeCol txInOut ma = divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) ma

stat :: MonadFront t m => TransStatus -> m ()
stat txStatus = case txStatus of
  TransConfirmed -> spanClass "history-page-status-icon" $ elClass "i" "fas fa-check fa-fw" $ blank
  TransUncofirmed -> spanClass "history-page-status-icon" $ elClass "i" "fas fa-question fa-fw" $ blank

transactionsGetting :: MonadFront t m => Currency -> m (Dynamic t [TransactionView], Dynamic t Word64)
transactionsGetting cur = do
  buildE <- delay 0.2 =<< getPostBuild
  settings <- getSettings
  pubStorageD <- getPubStorageD
  let getHeight pubStorage' = fromMaybe 0 $ _currencyPubStorage'height =<< Map.lookup cur (_pubStorage'currencyPubStorages pubStorage')
      heightD = getHeight <$> pubStorageD
      allBtcAddrsD = ffor pubStorageD $ \(PubStorage _ cm _ _) -> case Map.lookup BTC cm of
        Nothing -> []
        Just CurrencyPubStorage{..} -> V.toList $ extractAddrs _currencyPubStorage'pubKeystore
  timeZoneE <- getGetTimeZone buildE
  timeZoneD <- holdDyn utc timeZoneE
  let txListD = ffor2 allBtcAddrsD pubStorageD filterTx
      filterE = leftmost [tagPromptlyDyn txListD timeZoneE, updated txListD]
  filteredTxListE <- performFork $ ffor filterE $ \txs -> do
    timeZone <- sampleDyn timeZoneD
    pubStorage' <- sampleDyn pubStorageD
    getAndFilterBlocks heightD allBtcAddrsD timeZone txs pubStorage' settings
  filteredTxListD <- holdDyn [] filteredTxListE
  pure (filteredTxListD, heightD)
  where
    getAndFilterBlocks heightD btcAddrsD timeZone txs store settings = do
      allbtcAdrS <- sampleDyn btcAddrsD
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
                Just tx -> maybe 0 (\x -> hght - (fromMaybe 0 x) + 1) $ (fmap etxMetaHeight $ getBtcTxMeta tx)
              txParentsConfirmations = (fmap . fmap) getTxConfirmations parentTxs
              hasUnconfirmedParents = fmap (L.any (== 0)) txParentsConfirmations
          let rawTxsL = L.filter (\(a,b) -> a/=Nothing) $ L.zip bInOut $ txListRaw bl blh txs txsRefList hasUnconfirmedParents parentTxs
              prepTxs = L.sortOn txDate $ (prepareTransactionView allbtcAdrS hght timeZone (maybe btcDefaultExplorerUrls id $ Map.lookup cur (settingsExplorerUrl settings)) <$> rawTxsL)
          pure $ L.reverse $ addWalletState prepTxs

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

    txListRaw (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) = (TxRawInfo a b c d e f) : txListRaw as bs cs ds es fs

addWalletState :: [TransactionView] -> [TransactionView]
addWalletState txs = fmap setPrev $ fmap (\(prevTxCount, txView) -> (txView, calcAmount prevTxCount txs)) $ L.zip [0..] txs
  where
    setPrev (tr, prAm) = tr {txPrevAm = (Just (Money BTC prAm))}
    calcAmount n txs' = L.foldl' calc 0 $ L.take n txs'
    calc acc TransactionView{..} = if txInOut == TransRefill then acc + moneyAmount txAmount else acc - moneyAmount txAmount - (fromMaybe 0 $ moneyAmount <$> txFee txInfoView)

prepareTransactionView :: [EgvAddress] -> Word64 -> TimeZone -> ExplorerUrls -> (Maybe TransType, TxRawInfo) -> TransactionView
prepareTransactionView addrs hght tz sblUrl (mTT, TxRawInfo{..}) = TransactionView {
    txAmount = txAmountCalc
  , txPrevAm = Nothing
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
     ,txUrl           = blUrl <> "/tx/" <> txHex
     ,txFee           = txFeeCalc
     ,txConfirmations = bHeight
     ,txBlock         = txBlockLink
     ,txOutputs       = txOuts
     ,txInputs        = txInsOuts
    }
    btx = getBtcTx txr
    blHght = fromMaybe 0 $ maybe (Just 0) etxMetaHeight $ getBtcTxMeta txr
    bHeight = if ((blHght == 0) || (hght == 0))
      then 0
      else hght - blHght + 1
    txHs = HK.txHash btx
    txHex = HK.txHashToHex txHs

    txOuts = fmap (\out -> (txOutAdr out, Money BTC (HK.outValue out), TOUnspent, txOurArdCheck out)) $ HK.txOut btx
    txOutsAm = fmap (\out -> HK.outValue out) $ HK.txOut btx

    txOutsOurAm = fmap fst $ L.filter snd $ fmap (\out -> (HK.outValue out, not (txOurArdCheck out))) $ HK.txOut btx

    txInsOuts = fmap fst $ L.filter snd $ fmap (\out -> ((txOutAdr out, Money BTC (HK.outValue out)), txOurArdCheck out)) $ L.concat $ fmap (HK.txOut . getBtcTx) $ catMaybes txParents
    txInsOutsAm = fmap fst $ L.filter snd $ fmap (\out -> (HK.outValue out,txOurArdCheck out)) $ L.concat $ fmap (HK.txOut . getBtcTx) $ catMaybes txParents

    txOutAdr out = either (const Nothing) id $ (addrToString network) <$> (scriptToAddressBS $ HK.scriptOutput out)
    txOurArdCheck out = either (\_ -> False) (\a -> a `L.elem` btcAddrs) $ (scriptToAddressBS $ HK.scriptOutput out)

    network = getBtcNetwork $ getCurrencyNetwork BTC

    txBlockM = maybe Nothing (Just . HK.blockHashToHex) txHBl
    txBlockLink = maybe Nothing (\a -> Just (blUrl <> "/block/" <> a, a)) txBlockM

    blockTime = TxTime trTime -- $ maybe Nothing (Just . secToTimestamp . HK.blockTimestamp) txMBl
    trTime = fmap ((utcToZonedTime tz) . etxMetaTime ) $ getBtcTxMeta txr
    secToTimestamp t = utcToZonedTime tz $ posixSecondsToUTCTime $ fromIntegral t
    btcAddrs = fmap getBtcAddr addrs
    txFeeCalc = case mTT of
      Nothing -> Nothing
      Just TransRefill -> Nothing
      Just TransWithdraw -> Just $ Money BTC $ (sum txInsOutsAm) - (sum txOutsAm)
    txAmountCalc = case mTT of
      Nothing -> txom
      Just TransRefill -> txom
      Just TransWithdraw -> Money BTC $ sum txOutsOurAm

    blUrl = if isTestnet then testnetUrl sblUrl else mainnetUrl sblUrl

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
  , txPrevAm   :: Maybe Money
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
  , txBlock         :: Maybe (Text, Text)
  , txOutputs       :: [(Maybe Text, Money, TransOutputType, Bool)]
  , txInputs        :: [(Maybe Text, Money)]
} deriving (Show)
