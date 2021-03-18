{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.BumpFee(
    bumpFeePage
  ) where

import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Word

import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo.Btc
import Ergvein.Util
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Fee
import Ergvein.Wallet.Localization.History
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Transaction.Builder
import Ergvein.Wallet.Transaction.Util
import Ergvein.Wallet.Transaction.View
import Ergvein.Wallet.Widget.Input.BTC.Fee
import Ergvein.Wallet.Wrapper

import Network.Haskoin.Network (Inv(..), InvVector(..), InvType(..), Message(..))

import qualified Data.List as L
import qualified Data.Text as T
import qualified Network.Haskoin.Transaction as HT

bumpFeePage :: MonadFront t m => Currency -> TransactionView -> Maybe (BTCFeeMode, Word64) -> m ()
bumpFeePage cur  tr@TransactionView{..} mInit = do
  title <- localized BumpFeeTitle
  let thisWidget = Just $ pure $ bumpFeePage cur tr mInit
  wrapper False title thisWidget $ divClass "bump-fee-page" $ do
    elClass "h4" "mb-1" $ localizedText BumpFeeHeader
    newFeeRateE <- feeRateWidget cur tr mInit
    txDataE <- prepareTxData $ (txRaw txInfoView, ) <$> newFeeRateE
    txE <- makeRbfTx txDataE
    void $ nextWidget $ ffor txE $ \(txData, tx) -> Retractable {
        retractableNext = signSendWidget txData tx
      , retractablePrev = Just $ pure $ bumpFeePage cur tr $ Just (rbfTxData'feeMode txData, rbfTxData'feeRate txData)
      }

feeRateWidget :: MonadFront t m => Currency -> TransactionView -> Maybe (BTCFeeMode, Word64) -> m (Event t (BTCFeeMode, Word64))
feeRateWidget cur TransactionView{..} mInit = mdo
  let feeRate = calcFeeRate (txFee txInfoView) (txRaw txInfoView)
  moneyUnits <- fmap (fromMaybe defUnits . settingsUnits) getSettings
  makeBlock BumpFeeCurrentFee $ maybe BumpFeeFeeUnknown (\fee -> BumpFeeFeeAmount fee moneyUnits cur) $ txFee txInfoView
  makeBlock BumpFeeCurrentFeeRate $ maybe BumpFeeFeeRateUnknown (\fRate -> BumpFeeFeeRateAmount fRate cur) feeRate
  feeD <- btcFeeSelectionWidget BumpFeeNewFeeRateUnits mInit feeRate submitE
  submitE <- outlineButton CSSubmit
  let goE = attachWithMaybe (\mFeeRate _ -> mFeeRate) (current feeD) submitE
  pure goE

data RbfTxData = RbfTxData {
    rbfTxData'feeRate    :: !Word64                         -- ^ Fee rate in sat/vbyte
  , rbfTxData'feeMode    :: !BTCFeeMode                     -- ^ Fee mode
  , rbfTxData'change     :: !(Maybe (Word64, EgvPubKeyBox)) -- ^ Change amount and keybox to send the change to
  , rbfTxData'coins      :: ![UtxoPoint]                    -- ^ List of utxo points used as inputs
  , rbfTxData'outsToKeep :: ![(Text, Word64)]               -- ^ Fixed tx outputs in tx
  , rbfTxData'rbfEnabled :: !RbfEnabled                     -- ^ Explicit opt-in RBF signalling
} deriving Show

-- | Keep all inputs, keep all outputs that are not our change,
-- allow adding new inputs.
-- FIXME: localize errors
prepareTxData :: MonadFront t m => Event t (EgvTx, (BTCFeeMode, Word64)) -> m (Event t RbfTxData)
prepareTxData e = do
  pubStorage <- getPubStorage
  eDataE <- performEvent $ ffor e $ \(tx, (newFeeMode, newFeeRate)) -> do
    case tx of
      (TxErg _) -> pure $ Left BumpFeeErgoError
      (TxBtc txToReplace) -> do
        let btcTx = getBtcTx txToReplace
            inputs = HT.txIn btcTx
            outputs = HT.txOut btcTx
            replacedOutPoints = (\(i, _) -> HT.OutPoint (HT.txHash btcTx) i) <$> zip [0..] outputs
            inputsToKeep = inputs
        outputsToKeep <- liftIO $ flip runReaderT pubStorage $ extractOutputToKeep outputs
        mOldChangeOut <- liftIO $ flip runReaderT pubStorage $ extractChangeOutput outputs
        let mRecipientOutputTypes = getBtcOutputType <$> outputsToKeep
        case allJust mRecipientOutputTypes of
          Nothing -> pure $ Left BumpFeeGetOutTypeError
          Just recipientOutputTypes -> do
            let mChangeOutType = maybe (Just BtcP2WPKH) getBtcOutputType mOldChangeOut
            case mChangeOutType of
              Nothing -> pure $ Left BumpFeeGetChangeOutTypeError
              Just changeOutputType -> do
                eFixedUtxo <- liftIO $ flip runReaderT pubStorage $ getUtxoByInputs inputsToKeep
                case eFixedUtxo of
                  Left _ -> pure $ Left BumpFeeGetUtxoError
                  Right fixedUtxo -> do
                    let amount = L.sum $ HT.outValue <$> outputsToKeep
                        outputTypes = changeOutputType : recipientOutputTypes
                        (confirmedUtxo', unconfirmedUtxo') = getBtcUtxoPointsParted pubStorage
                        -- here we remove outputs that was created in replaced transaction
                        -- from our set of available outputs
                        removeBadOutputs outs = filter (\out -> not $ L.elem (upPoint out) replacedOutPoints) outs
                        (confirmedUtxo, unconfirmedUtxo) = (removeBadOutputs confirmedUtxo', removeBadOutputs unconfirmedUtxo')
                    case chooseCoinsRbf amount newFeeRate outputTypes fixedUtxo confirmedUtxo unconfirmedUtxo of
                      Left _ -> pure $ Left BumpFeeInsufficientFundsError
                      Right (coins, change) -> do
                        let lastUnusedKey =  snd <$> (getLastUnusedKey Internal =<< pubStorageKeyStorage BTC pubStorage)
                            getOldChangeKey oldChangeOut = (flip getInternalKeyboxByOutput) oldChangeOut =<< pubStorageKeyStorage BTC pubStorage
                            mChangeKey = maybe lastUnusedKey getOldChangeKey mOldChangeOut
                        case mChangeKey of
                          Nothing -> pure $ Left BumpFeeGetChangeKeyError
                          Just changeKey -> do
                            let mUnpackedOutsToKeep = unpackOut <$> outputsToKeep
                            case allJust mUnpackedOutsToKeep of
                              Nothing -> pure $ Left BumpFeeDecodeOutsError
                              Just decodedOutsToKeep -> pure $ Right RbfTxData {
                                  rbfTxData'feeRate = newFeeRate
                                , rbfTxData'feeMode = newFeeMode
                                , rbfTxData'change = Just (change, changeKey)
                                , rbfTxData'coins = coins
                                , rbfTxData'outsToKeep = decodedOutsToKeep
                                , rbfTxData'rbfEnabled = True -- TODO: add checkbox for this in UI
                                }
  dataE <- handleDangerMsg eDataE
  pure dataE

chooseCoinsRbf :: Word64 -> Word64 -> [BtcAddressType] -> [UtxoPoint] -> [UtxoPoint] -> [UtxoPoint] -> Either Text ([UtxoPoint], Word64)
chooseCoinsRbf amount newFeeRate outputTypes fixedUtxo confirmedUtxo unconfirmedUtxo =
  let
    firstpick = chooseCoins amount newFeeRate outputTypes (Just fixedUtxo) True $ L.sort confirmedUtxo
    finalpick = either (const $ chooseCoins amount newFeeRate outputTypes (Just fixedUtxo) True $ L.sort $ confirmedUtxo <> unconfirmedUtxo) Right firstpick
  in
    case finalpick of
      Left e -> Left $ T.pack e
      Right pick -> Right pick

-- Removes all change addrs from the given list.
extractOutputToKeep :: (HasPubStorage m, PlatformNatives) => [HT.TxOut] -> m [HT.TxOut]
extractOutputToKeep outputs = do
  pubStorage <- askPubStorage
  let ourBtcChangeAddrs = getChangeBtcAddrs pubStorage
  outputsToKeep <- filterM ((fmap not) . checkOutIsOursBtc ourBtcChangeAddrs) outputs
  pure outputsToKeep

-- Here we extract change output from the tx that we want to replace.
-- Even if there are more than one change output, the change will be sent to the first of these outputs in a new transaction.
extractChangeOutput :: (HasPubStorage m, PlatformNatives) => [HT.TxOut] -> m (Maybe HT.TxOut)
extractChangeOutput outputs = do
  pubStorage <- askPubStorage
  let ourBtcChangeAddrs = getChangeBtcAddrs pubStorage
  oldChangeOuts <- filterM (checkOutIsOursBtc ourBtcChangeAddrs) outputs
  pure $ fst <$> L.uncons oldChangeOuts

calcFeeRate :: Maybe Money -> EgvTx -> Maybe Rational
calcFeeRate (Just money) (TxBtc btcTx) =
  let txVsize = calcTxVsize $ getBtcTx btcTx
      fee = moneyToRationalUnit money smallestUnits
  in Just $ fee / (fromIntegral txVsize)
calcFeeRate (Just _) (TxErg _) = Nothing -- TODO: implement for ERGO
calcFeeRate _ _ = Nothing

makeBlock :: (MonadFront t m, LocalizedPrint l) => l -> l -> m ()
makeBlock title content = divClass "mb-1" $ do
  elClass "span" "font-bold" $ localizedText title
  br
  localizedText content

makeRbfTx :: MonadFront t m => Event t RbfTxData -> m (Event t (RbfTxData, HT.Tx))
makeRbfTx txDataE = do
  let eTxE = ffor txDataE $ \txData@RbfTxData{..} ->
        let keyToText = btcAddrToString . xPubToBtcAddr . extractXPubKeyFromEgv . pubKeyBox'key
            changeOutputs = maybe [] (\(change, changeKey) -> [(keyToText changeKey, change)]) rbfTxData'change
            outputsToKeep = rbfTxData'outsToKeep
            outs = outputsToKeep ++ changeOutputs
            eTx = buildAddrTx btcNetwork rbfTxData'rbfEnabled (upPoint <$> rbfTxData'coins) outs
        in (txData, ) <$> first (const BumpFeeInvalidAddressError) eTx
  txE <- handleDangerMsg eTxE
  pure txE

signSendWidget :: MonadFront t m => RbfTxData -> HT.Tx -> m ()
signSendWidget txData@RbfTxData{..} tx = do
  title <- localized BumpFeeTitle
  let thisWidget = Just $ pure $ signSendWidget txData tx
  wrapper False title thisWidget $ divClass "bump-fee-page" $ mdo
    let titleD = (\newTitle -> if newTitle then BumpFeeTxPostedHeader else BumpFeeConfirmTxHeader) <$> displayNewTitleD
    elClass "h4" "mb-1" $ localizedDynText titleD
    let
      inputsAmount = sum $ (btcUtxo'amount . upMeta) <$> rbfTxData'coins
      outputsAmount = sum $ HT.outValue <$> HT.txOut tx
      fee = inputsAmount - outputsAmount
    makeBlock BumpFeeNewFee $ BumpFeeFeeAmount (Money BTC fee) smallestUnits BTC
    makeBlock BumpFeeNewFeeRate $ BumpFeeNewFeeRateAmount rbfTxData'feeRate BTC
    displayNewTitleD <- workflow $ signTx tx rbfTxData'coins
    pure ()

signTx :: MonadFront t m => HT.Tx -> [UtxoPoint] -> Workflow t m Bool
signTx tx coins = Workflow $ do
  signE <- outlineButton BumpFeeSignTx
  eSignedTxE <- fmap (fmapMaybe id) $ withWallet $ (signTxWithWallet tx coins) <$ signE
  signedTxE <- handleDangerMsg $ first (const BumpFeeSignError) <$> eSignedTxE
  pure (False, sendTx <$> signedTxE)

sendTx :: MonadFront t m =>  HT.Tx -> Workflow t m Bool
sendTx signedTx = Workflow $ do
  sendE <- outlineButton BumpFeeSendTx
  addedE <- addOutgoingTx "signSendWidget" $ (TxBtc $ BtcTx signedTx Nothing) <$ sendE
  storedE <- btcMempoolTxInserter $ signedTx <$ addedE
  broadcastE <- requestBroadcast $ ffor storedE $ const $
    NodeReqBTC . MInv . Inv . pure . InvVector InvTx . HT.getTxHash . HT.txHash $ signedTx
  pure (False, showTxId signedTx <$ broadcastE)

showTxId :: MonadFront t m => HT.Tx -> Workflow t m Bool
showTxId tx = Workflow $ do
  divClass "mb-1" $ do
    elClass "span" "font-bold" $ localizedText BumpFeeTxId
    br
    elClass "span" "word-break-all" $ makeTxIdLink $ btcTxHashToStr $ HT.txHash tx
  backE <- outlineButton CSBack
  void $ nextWidget $ ffor backE $ const $ Retractable {
      retractableNext = balancesPage
    , retractablePrev = Nothing
  }
  pure (True, never)

makeTxIdLink :: MonadFront t m => Text -> m ()
makeTxIdLink txIdText = do
  settings <- getSettings
  let urlPrefixes = btcSettings'explorerUrls $ getBtcSettings settings
      urlPrefix = if isTestnet then testnetUrl urlPrefixes else mainnetUrl urlPrefixes
  hyperlink "link" txIdText (urlPrefix <> "/tx/" <> txIdText)