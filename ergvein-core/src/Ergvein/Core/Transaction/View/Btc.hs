{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.View.Btc(
    TxInfo(..)
  , TxView(..)
  , TxDetailedView(..)
  , checkAddrInOut
  , addWalletState
  , prepareTxView
) where

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time
import Data.Word
import Network.Haskoin.Address
import Reflex

import Ergvein.Core.Platform
import Ergvein.Core.Settings
import Ergvein.Core.Store.Monad
import Ergvein.Core.Transaction.Btc
import Ergvein.Core.Transaction.View.Common
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Network
import Ergvein.Types.Transaction
import Sepulcas.Native

import qualified Data.List as L
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Transaction        as HK

-- The data type that contains the information needed to display the transaction on the screen.
data TxInfo = TxInfo {
    txInfo'blockHeader           :: Maybe HK.BlockHeader -- The header of a block that this tx was included in. Nothing means that this tx is unconfirmed.
  , txInfo'blockHash             :: Maybe HK.BlockHash -- The hash of a block that this tx was included in. Nothing means that this tx is unconfirmed.
  , txInfo'tx                    :: BtcTx -- Transaction itself.
  , txInfo'amount                :: Money -- Transaction amount.
  , txInfo'hasUnconfirmedParents :: Bool -- Flag indicating the presence of unconfirmed parent transactions.
  , txInfo'outStatuses           :: [TransOutputType] -- Status of transaction outputs (whether they were spent or not) in the same order they are in the transaction.
  , txInfo'spentOutputs          :: [Maybe HK.TxOut] -- Outputs that were spent by this transaction.
  , txInfo'conflictingTxs        :: [TxId] -- Transactions that conflict with this transaction (e.g. double spending transactions).
  , txInfo'replacedTxs           :: [TxId] -- Transactions that were replaced by this transaction.
  , txInfo'possReplacedTxs       :: (Bool, [TxId]) --Transactions that may have been replaced by this transaction or that are replacing this transaction. If the first element of the pair is True then this tx is replacing txs listed in the second element of the pair, otherwise it is replaced by given txs.
} deriving (Show)

-- The data type that contains the information needed to display the transaction in compact form on the transaction history page.
data TxView = TxView {
    txView'amount             :: Money -- Transaction amount.
  , txView'prevAmount         :: Maybe Money -- Wallet balance before receiving this transaction.
  , txView'time               :: TxTime -- Time when the transaction was received.
  , txView'inOut              :: TransType -- Flag indicating an incoming or outgoing transaction.
  , txView'detailedView       :: TxDetailedView -- This field is required for redirects to the tx info page.
  , txView'confirmationStatus :: TxConfirmationStatus -- Confirmation status of the transaction.
} deriving (Show)

-- The data type that contains the information needed to display transaction details on the transaction info page.
data TxDetailedView = TxDetailedView {
    txDetailedView'txId                :: Text
  , txDetailedView'tx                  :: BtcTx
  , txDetailedView'label               :: Maybe Text
  , txDetailedView'explorerUrl         :: Text
  , txDetailedView'fee                 :: Maybe Money
  , txDetailedView'rbfEnabled          :: Bool
  , txDetailedView'conflictingTxs      :: [TxId]
  , txDetailedView'replacedTxs         :: [TxId]
  , txDetailedView'possiblyReplacedTxs :: (Bool, [TxId])
  , txDetailedView'confirmations       :: Word64
  , txDetailedView'block               :: Maybe (Text, Text)
  , txDetailedView'outputs             :: [(Maybe Text, Money, TransOutputType, Bool)]
  , txDetailedView'inputs              :: [(Maybe Text, Money)]
} deriving (Show)

checkAddrInOut :: (HasTxStorage m, PlatformNatives) => [BtcAddress] -> BtcTx -> m (Maybe TransType)
checkAddrInOut btcAddrs tx = do
  bLIn <- traverse (`checkAddrTxInBtc` getBtcTx tx) btcAddrs
  bLOut <- traverse (`checkAddrTxOutBtc` getBtcTx tx) btcAddrs
  let isIn = L.or bLIn
      isOut = L.or bLOut
  if not (isIn || isOut)
    then pure Nothing
    else if isIn
      then pure $ Just TransWithdraw
      else pure $ Just TransRefill

addWalletState :: [TxView] -> [TxView]
addWalletState txs = fmap (setPrev . (\(prevTxCount, txView) -> (txView, calcAmount prevTxCount txs))) (L.zip [0..] txs)
  where
    setPrev (tr, prAm) = tr {txView'prevAmount = Just $ Money BTC prAm}
    calcAmount n txs' = L.foldl' calc 0 $ L.take n txs'
    calc acc TxView{..} = case txView'inOut of
      TransRefill -> acc + moneyAmount txView'amount
      TransWithdraw -> acc - moneyAmount txView'amount - maybe 0 moneyAmount (txDetailedView'fee txView'detailedView)

prepareTxView ::
  [BtcAddress] ->
  Word64 ->
  TimeZone ->
  ExplorerUrls ->
  (Maybe TransType, TxInfo) ->
  TxView
prepareTxView addrs hght tz sblUrl (mTT, TxInfo{..}) = btcView txInfo'tx
  where
    btcView (BtcTx btx meta) = TxView {
        txView'amount = txAmountCalc
      , txView'prevAmount = Nothing
      , txView'time = blockTime
      , txView'inOut = fromMaybe TransRefill mTT
      , txView'detailedView = txInf
      , txView'confirmationStatus =
          if txInfo'hasUnconfirmedParents && (bHeight == 0)
            then TransUncofirmedParents
          else if bHeight == 0
            then TransUncofirmed
          else TransConfirmed
      }
      where
        txInf = TxDetailedView {
            txDetailedView'txId                = txHex
          , txDetailedView'tx                  = txInfo'tx
          , txDetailedView'label               = Nothing
          , txDetailedView'explorerUrl         = blUrl <> "/tx/" <> txHex
          , txDetailedView'fee                 = txFeeCalc
          , txDetailedView'rbfEnabled          = markedReplaceable btx
          , txDetailedView'conflictingTxs      = txInfo'conflictingTxs
          , txDetailedView'replacedTxs         = txInfo'replacedTxs
          , txDetailedView'possiblyReplacedTxs = txInfo'possReplacedTxs
          , txDetailedView'confirmations       = bHeight
          , txDetailedView'block               = txBlockLink
          , txDetailedView'outputs             = txOuts
          , txDetailedView'inputs              = txIns
        }
        blHght = fromMaybe 0 $ maybe (Just 0) etxMetaHeight meta
        bHeight = if (blHght == 0) || (hght == 0)
          then 0
          else hght - blHght + 1
        txHs = HK.txHash btx
        txHex = HK.txHashToHex txHs

        txOuts = (\(out, outStatus) -> (txOutAdr out, Money BTC (HK.outValue out), outStatus, txOurAdrCheck out)) <$> L.zip (HK.txOut btx) txInfo'outStatuses
        txOutsAm = HK.outValue <$> HK.txOut btx

        txOutsOurAm = fmap fst $ L.filter snd $ (\out -> (HK.outValue out, not (txOurAdrCheck out))) <$> HK.txOut btx

        txIns = mapMaybe (maybe Nothing (\out -> Just (txOutAdr out, Money BTC (HK.outValue out)))) txInfo'spentOutputs
        txInsAm = mapMaybe (fmap HK.outValue) txInfo'spentOutputs

        txOutAdr out = fromRight Nothing $ addrToString network <$> scriptToAddressBS (HK.scriptOutput out)
        txOurAdrCheck out = either (const False) (`L.elem` addrs) (scriptToAddressBS $ HK.scriptOutput out)

        network = getBtcNetwork $ getCurrencyNetwork BTC

        txBlockM = HK.blockHashToHex <$> txInfo'blockHash
        txBlockLink = (\a -> Just (blUrl <> "/block/" <> a, a)) =<< txBlockM

        blockTime = TxTime trTime -- $ maybe Nothing (Just . secToTimestamp . HK.blockTimestamp) txInfo'blockHeader
        trTime = fmap (utcToZonedTime tz . etxMetaTime) meta
        txFeeCalc = case mTT of
          Nothing -> Nothing
          Just TransRefill -> Nothing
          Just TransWithdraw -> Just $ Money BTC $ sum txInsAm - sum txOutsAm
        txAmountCalc = case mTT of
          Nothing -> txInfo'amount
          Just TransRefill -> txInfo'amount
          Just TransWithdraw -> Money BTC $ sum txOutsOurAm

        blUrl = if isTestnet then testnetUrl sblUrl else mainnetUrl sblUrl
