module Ergvein.Wallet.Transaction.View(
    ExpStatus(..)
  , TransStatus(..)
  , TransType(..)
  , TransOutputType(..)
  , TxRawInfo(..)
  , TxTime(..)
  , TransactionView(..)
  , TransactionViewInfo(..)
  , checkAddrInOut
  , addWalletState
  , prepareTransactionView
  ) where

import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Text as T
import Data.Time
import Data.Word
import Network.Haskoin.Address

import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Network
import Ergvein.Types.Transaction
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Transaction.Util

import qualified Data.List as L
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Transaction        as HK

-- Front types, should be moved to Utils
data ExpStatus = Expanded | Minified deriving (Eq, Show)

data TransStatus = TransUncofirmed | TransUncofirmedParents | TransConfirmed deriving (Eq, Show)

data TransType = TransRefill | TransWithdraw deriving (Eq, Show)

data TransOutputType = TOSpent | TOUnspent | TOUnknown deriving (Eq, Show)

instance LocalizedPrint TransOutputType where
  localizedShow l v = case l of
    English -> case v of
      TOSpent   -> "Spent"
      TOUnspent -> "Unspent"
      TOUnknown -> "Unknown"
    Russian -> case v of
      TOSpent   -> "Потрачен"
      TOUnspent -> "Не потрачен"
      TOUnknown -> "Неизвестно"

data TxRawInfo = TxRawInfo {
    txMBl                   :: Maybe HK.BlockHeader
  , txHBl                   :: Maybe HK.BlockHash
  , txr                     :: EgvTx
  , txom                    :: Money
  , txHasUnconfirmedParents :: Bool
  , txParents               :: [Maybe EgvTx]
  , txOutsStatuses          :: [TransOutputType]
  , txConflTxs              :: [TxId]
  , txReplTxs               :: [TxId]
  , txPossReplTxs           :: (Bool, [TxId])
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
    txAmount         :: Money
  , txPrevAm         :: Maybe Money
  , txDate           :: TxTime
  , txInOut          :: TransType
  , txInfoView       :: TransactionViewInfo
  , txStatus         :: TransStatus
} deriving (Show)

data TransactionViewInfo = TransactionViewInfo {
    txId                  :: Text
  , txLabel               :: Maybe Text
  , txUrl                 :: Text
  , txFee                 :: Maybe Money
  , txRbfEnabled          :: Bool
  , txConflictingTxs      :: [TxId]
  , txReplacedTxs         :: [TxId]
  , txPossiblyReplacedTxs :: (Bool, [TxId])
  , txConfirmations       :: Word64
  , txBlock               :: Maybe (Text, Text)
  , txOutputs             :: [(Maybe Text, Money, TransOutputType, Bool)]
  , txInputs              :: [(Maybe Text, Money)]
} deriving (Show)

checkAddrInOut :: (HasTxStorage m, PlatformNatives) => [EgvAddress] -> EgvTx -> m (Maybe TransType)
checkAddrInOut ac (TxBtc tx) = do
  let btcAddrs = mapMaybe (\addr -> case addr of (BtcAddress addr) -> Just addr; _ -> Nothing) ac
  bLIn <- traverse (flip checkAddrTxInBtc (getBtcTx tx)) btcAddrs
  bLOut <- traverse (flip checkAddrTxOutBtc (getBtcTx tx)) btcAddrs
  let isIn = L.or bLIn
      isOut = L.or bLOut
  if (not (isIn || isOut))
    then pure Nothing
    else if isIn
      then pure $ Just TransWithdraw
      else pure $ Just TransRefill
checkAddrInOut _ (TxErg _) = error "checkAddrInOut: Ergo is not implemented!"

addWalletState :: [TransactionView] -> [TransactionView]
addWalletState txs = fmap setPrev $ fmap (\(prevTxCount, txView) -> (txView, calcAmount prevTxCount txs)) $ L.zip [0..] txs
  where
    setPrev (tr, prAm) = tr {txPrevAm = (Just (Money BTC prAm))}
    calcAmount n txs' = L.foldl' calc 0 $ L.take n txs'
    calc acc TransactionView{..} = if txInOut == TransRefill then acc + moneyAmount txAmount else acc - moneyAmount txAmount - (fromMaybe 0 $ moneyAmount <$> txFee txInfoView)

prepareTransactionView ::
  [EgvAddress] ->
  Word64 ->
  TimeZone ->
  ExplorerUrls ->
  (Maybe TransType, TxRawInfo) ->
  TransactionView
prepareTransactionView addrs hght tz sblUrl (mTT, TxRawInfo{..}) = case txr of
  TxBtc btx -> btcView btx
  TxErg _ -> error "prepareTransactionView: Ergo is not impolemented"
  where
    btcView (BtcTx btx meta) = TransactionView {
        txAmount = txAmountCalc
      , txPrevAm = Nothing
      , txDate = blockTime
      , txInOut = fromMaybe TransRefill mTT
      , txInfoView = txInf
      , txStatus =
          if (txHasUnconfirmedParents) && (bHeight == 0)
            then TransUncofirmedParents
          else if (bHeight == 0)
            then TransUncofirmed
          else TransConfirmed
      }
      where
        txInf = TransactionViewInfo {
            txId                  = txHex
          , txLabel               = Nothing
          , txUrl                 = blUrl <> "/tx/" <> txHex
          , txFee                 = txFeeCalc
          , txRbfEnabled          = markedReplaceable btx
          , txConflictingTxs      = txConflTxs
          , txReplacedTxs         = txReplTxs
          , txPossiblyReplacedTxs = txPossReplTxs
          , txConfirmations       = bHeight
          , txBlock               = txBlockLink
          , txOutputs             = txOuts
          , txInputs              = txInsOuts
        }
        blHght = fromMaybe 0 $ maybe (Just 0) etxMetaHeight meta
        bHeight = if ((blHght == 0) || (hght == 0))
          then 0
          else hght - blHght + 1
        txHs = HK.txHash btx
        txHex = HK.txHashToHex txHs

        txOuts = fmap (\(out, outStatus) -> (txOutAdr out, Money BTC (HK.outValue out), outStatus, txOurAdrCheck out)) $ L.zip (HK.txOut btx) txOutsStatuses
        txOutsAm = fmap (\out -> HK.outValue out) $ HK.txOut btx

        txOutsOurAm = fmap fst $ L.filter snd $ fmap (\out -> (HK.outValue out, not (txOurAdrCheck out))) $ HK.txOut btx

        getOut = HK.txOut . getBtcTx . fromJust . toTxBtc
        txInsOuts = fmap fst $ L.filter snd $ fmap (\out -> ((txOutAdr out, Money BTC (HK.outValue out)), txOurAdrCheck out)) $ L.concat $ fmap getOut $ catMaybes txParents
        txInsOutsAm = fmap fst $ L.filter snd $ fmap (\out -> (HK.outValue out,txOurAdrCheck out)) $ L.concat $ fmap getOut $ catMaybes txParents

        txOutAdr out = either (const Nothing) id $ (addrToString network) <$> (scriptToAddressBS $ HK.scriptOutput out)
        txOurAdrCheck out = either (\_ -> False) (\a -> a `L.elem` btcAddrs) $ (scriptToAddressBS $ HK.scriptOutput out)

        network = getBtcNetwork $ getCurrencyNetwork BTC

        txBlockM = maybe Nothing (Just . HK.blockHashToHex) txHBl
        txBlockLink = maybe Nothing (\a -> Just (blUrl <> "/block/" <> a, a)) txBlockM

        blockTime = TxTime trTime -- $ maybe Nothing (Just . secToTimestamp . HK.blockTimestamp) txMBl
        trTime = fmap ((utcToZonedTime tz) . etxMetaTime ) meta
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
