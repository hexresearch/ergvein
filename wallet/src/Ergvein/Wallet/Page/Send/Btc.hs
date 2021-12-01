{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Page.Send.Btc (
    sendPageBtc
  ) where

import Control.Lens
import Control.Monad.Except
import Data.Either (fromLeft)
import Data.Maybe
import Data.Word

import Ergvein.Text
import Ergvein.Types
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Orphanage ()
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.TxInfo.Common
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Widget.Input.Amount
import Ergvein.Wallet.Widget.Input.Fee
import Ergvein.Wallet.Widget.Input.Recipient
import Ergvein.Wallet.Wrapper
import Sepulcas.Alert (handleDangerMsg)
import Sepulcas.Elements
import Sepulcas.Elements.Toggle
import Sepulcas.Text (Display(..))
import Sepulcas.Validate

import Network.Haskoin.Network (Inv(..), InvVector(..), InvType(..), Message(..))

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Network.Haskoin.Transaction as HT

data UserInput = UserInput {
    userInput'address :: BtcAddress
  , userInput'amount :: (Word64, UnitBTC)
  , userInput'fee :: (Word64, FeeMode)
  , userInput'rbfEnabled :: RbfEnabled
} deriving (Eq, Show)

validateAmountHelper :: Reflex t => Dynamic t UnitBTC -> Word64 -> Text -> PushM t (Either [ValidationError] Word64)
validateAmountHelper unitD threshold amount = do
  unit <- sampleDyn unitD
  pure $ toEither $ validateAmount threshold unit amount

-- | Returns maximum available balance to send in satoshis.
calcMaxAvailableAmount :: PubStorage -> Word64 -> BtcAddress-> Word64
calcMaxAvailableAmount pubStorage feeRate recipient =
  let utxos = filter (not . isSendingUtxo . btcUtxo'status) $ M.elems $ getBtcUtxos pubStorage
      maxSpendableAmount = sum $ btcUtxo'amount <$> utxos
      inTypes = btcScriptOutputToAddressType . btcUtxo'script <$> utxos
      -- Since all the money is being sent, no change is needed.
      outTypes = [btcAddrToBtcOutType recipient]
      fee = guessTxFee feeRate outTypes inTypes
  in if maxSpendableAmount > fee then maxSpendableAmount - fee else 0

isSendingUtxo :: EgvUtxoStatus -> Bool
isSendingUtxo (EUtxoSending _) = True
isSendingUtxo _ = False

mkAmountErrsDyn :: (MonadReflex t m, LocalizedPrint l)
  => Event t () -- ^ Event that triggers validation
  -> Event t [l]
  -> Dynamic t Text -- ^ Dynamic with input text
  -> (Text -> PushM t (Either [l] b)) -- ^ Validator
  -> m (Dynamic t [l])
mkAmountErrsDyn submitE sendAllErrsE inputD validator = holdDyn [] (leftmost [errsE, sendAllErrsE]) where
  inputE = poke submitE $ const $ sampleDyn inputD
  errsE = fromLeft [] <$> poke inputE validator

sendPageBtc :: MonadFront t m
  => Maybe UserInput
  -> m ()
sendPageBtc mInitInput = mdo
  walletName <- getWalletName
  title <- localized walletName
  let navbar = if isAndroid
        then blank
        else navbarWidget BTC thisWidget NavbarSend
      thisWidget = Just $ sendPageBtc <$> infoD
  infoD <- wrapperNavbar False title thisWidget navbar $ divClass "send-page" $ mdo
    settings <- getSettings
    let mAmountInit = userInput'amount <$> mInitInput
        mFeeInit = userInput'fee <$> mInitInput
        mRecipientInit = userInput'address <$> mInitInput
        mRbfInit = userInput'rbfEnabled <$> mInitInput
        rbfFromSettings = btcSettings'sendRbfByDefault $ getBtcSettings settings
        rbfInit' = fromMaybe rbfFromSettings mRbfInit
    retInfoD <- formClass "mb-0" $ mdo
      pubStorageD <- getPubStorageD
      let
        sendAllResultE = poke sendAllBtnE $ const $ do
          pubStorage <- sampleDyn pubStorageD
          recipientText <- sampleDyn recipientD
          feeRateText <- sampleDyn feeRateD
          let mRecipient = eitherToMaybe $ toEither $ validate recipientText
              mFeeRate = eitherToMaybe $ toEither $ validateFeeRate BTC Nothing feeRateText
          case (mRecipient, mFeeRate) of
            (Just recipient, Just feeRate) -> do
              pure $ Right $ calcMaxAvailableAmount pubStorage feeRate recipient
            _  -> pure $ Left [SendAllErr]
        setAmountSatE = fmapMaybe eitherToMaybe sendAllResultE
        sendAllErrsE = fromLeft [] <$> sendAllResultE
        setAmountE = poke setAmountSatE $ \amount -> do
          u <- sampleDyn amountUnitD
          pure $ showMoneyUnit (Money BTC amount) u
      -- A delay of 0.05 gives the dynamics time to update before validation
      autoFeeModeE <- delay 0.05 $ void $ ffilter (/= FeeModeManual) $ updated feeModeD
      setRecipientNextFrameE <- delay 0.05 setRecipientE
      recipientErrsD <- mkErrsDyn (leftmost [submitE, setRecipientNextFrameE]) recipientD (pure . toEither . (validate :: Text -> Validation [ValidationError] BtcAddress))
      amountErrsD <- mkAmountErrsDyn submitE sendAllErrsE amountD (validateAmountHelper amountUnitD 0)
      feeRateErrsD <- mkErrsDyn (leftmost [submitE, autoFeeModeE]) feeRateD (pure . toEither . validateFeeRate BTC Nothing)
      (recipientD, setRecipientE) <- recipientWidget BTC mRecipientInit recipientErrsD
      (amountD, amountUnitD, sendAllBtnE) <- sendAmountWidgetBtc mAmountInit setAmountE amountErrsD
      (feeRateD, feeModeD) <- feeSelectionWidgetBtc (FSRate BTC) mFeeInit feeRateErrsD
      rbfEnabledD <- divClass "mb-2" $ do
        label "" $ localizedText SSRbf
        toggler $ pure rbfInit'
      submitE <- outlineSubmitTextIconButtonClass "w-100 mb-0" SendBtnString "fas fa-paper-plane fa-lg"
      let
        userInputE = flip push submitE $ \_ -> do
          recipientText <- sampleDyn recipientD
          amountText <- sampleDyn amountD
          unit <- sampleDyn amountUnitD
          feeRateText <- sampleDyn feeRateD
          feeMode <- sampleDyn feeModeD
          rbfEnabled <- sampleDyn rbfEnabledD
          let
            eRecipient = toEither $ validate recipientText
            eAmount = toEither $ validateAmount 0 unit amountText
            eFeeRate = toEither $ validateFeeRate BTC Nothing feeRateText
          case (eRecipient, eAmount, eFeeRate) of
            (Right recipient, Right amount, Right feeRate) -> pure $ Just
              UserInput {
                userInput'address = recipient,
                userInput'amount = (amount, unit),
                userInput'fee = (feeRate, feeMode),
                userInput'rbfEnabled = rbfEnabled
              }
            _ -> pure Nothing
      eTxInfoE <- makeTx userInputE
      txInfoE <- handleDangerMsg eTxInfoE
      void $ nextWidget $ ffor txInfoE $ \(userInput, pick, totalFee, tx) -> Retractable {
          retractableNext = confirmationPage userInput pick totalFee tx
        , retractablePrev = Just $ pure $ sendPageBtc $ Just userInput
        }
      holdDyn mInitInput $ (\(userInput, _, _, _) -> Just userInput) <$> txInfoE
    pure retInfoD
  pure ()

makeTx :: MonadFront t m
  => Event t (UserInput)
  -> m (Event t (Either ConfirmationErrorMessage (UserInput, [UtxoPoint], Word64, HT.Tx)))
makeTx userInputE = do
  pubStorageD <- getPubStorageD
  performFork $ ffor userInputE $ \userInput@UserInput{..} -> do
    ps <- sampleDyn pubStorageD
    let utxo = ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'utxos
        mChangeKey = getLastUnusedKey Internal =<< pubStorageKeyStorage BTC ps
    case (utxo, mChangeKey) of
      (Nothing, _) -> pure $ Left CEMEmptyUTXO
      (_, Nothing) -> pure $ Left CEMNoChangeKey
      (Just _, Just (_, changeKey)) -> do
        let outputTypes = [btcAddrToBtcOutType userInput'address]
            feeRate = fst userInput'fee
            amount = fst userInput'amount
            changeOutType = BtcP2WPKH
            (confs, unconfs) = getBtcUtxoPointsParted ps
            firstpick = chooseCoins amount feeRate outputTypes changeOutType Nothing $ L.sort confs
            finalpick = either (const $ chooseCoins amount feeRate outputTypes changeOutType Nothing $ L.sort $ confs <> unconfs) Right firstpick
        pure $ either' finalpick (const $ Left CEMNoSolution) $ \(pick, mChange) ->
          let
            keyBoxToTxt key = btcAddrToText $ xPubToBtcAddr $ extractXPubKeyFromEgv $ pubKeyBox'key key
            outs = case mChange of
              Nothing -> [(btcAddrToText userInput'address, amount)]
              Just change -> [(btcAddrToText userInput'address, amount), (keyBoxToTxt changeKey, change)]
            inputsAmount = sum $ btcUtxo'amount . upMeta <$> pick
            outputsAmount = case mChange of
              Nothing -> amount
              Just change -> amount + change
            totalFee = inputsAmount - outputsAmount
            eTx = buildAddrTx btcNetwork userInput'rbfEnabled (upPoint <$> pick) outs
          in either (const $ Left CEMTxBuildFail) (\tx -> Right (userInput, pick, totalFee, tx)) eTx
  where
    either' e l r = either l r e

mkrow :: (MonadFront t m, LocalizedPrint l) => l -> m b -> Bool -> m ()
mkrow a mb wordBreak = el "div" $ do
  elClass "span" "font-bold" $ do
    localizedText a
    text ": "
  let wordBreakClass = if wordBreak then "word-break-all" else ""
  void $ elClass "span" wordBreakClass mb

-- | Simply displays the relevant information about a transaction
confirmationPage :: MonadFront t m => UserInput -> [UtxoPoint] -> Word64 -> HT.Tx -> m ()
confirmationPage userInput@UserInput{..} pick totalFee tx = do
  title <- localized SSConfirm
  let thisWidget = Just $ pure $ confirmationPage userInput pick totalFee tx
  void $ wrapper False title thisWidget $ divClass "ta-l" $ mdo
    let amount = fst userInput'amount
        unit = snd userInput'amount
    mkrow AmountString (text $ showMoneyUnit (Money BTC amount) unit <> " " <> display unit) False
    mkrow RecipientString (text $ btcAddrToText userInput'address) True
    mkrow SSFee (text $ showt totalFee <> " " <> display BtcSat) False
    mkrow SSRbf (localizedText $ FSRbf userInput'rbfEnabled) False
    mkrow SSTotal (text $ showMoneyUnit (Money BTC $ amount + totalFee) unit <> " " <> display unit) False
    -- Sign transaction
    showSignBtnD <- holdDyn True (False <$ signBtnE)
    signBtnE <- networkHoldDynE $ ffor showSignBtnD $ \showSignBtn ->
      if not showSignBtn
        then pure never
        else divClass "mt-1" $ outlineButton SendBtnSign
    eSignedTxE <- fmap (fmapMaybe id) $ withWallet $ signTxWithWallet tx pick <$ signBtnE
    signedTxE <- handleDangerMsg $ either (const $ Left CEMSignFail) Right <$> eSignedTxE
    networkHold_ (pure ()) $ ffor signedTxE $ \signedTx -> mdo
      -- Send transaction
      showSendBtnD <- holdDyn True (False <$ sendBtnE)
      sendBtnE <- networkHoldDynE $ ffor showSendBtnD $ \showSendBtn ->
        if not showSendBtn
          then pure never
          else divClass "mt-1" $ outlineButton SendBtnSend
      addedE <- addOutgoingTx "confirmationPage" $ (TxBtc (BtcTx signedTx Nothing)) <$ sendBtnE
      storedE <- btcMempoolTxInserter $ signedTx <$ addedE
      broadcastedE <- requestBroadcast $ ffor storedE $ const $
        NodeReqBtc . MInv . Inv . pure . InvVector InvTx . HT.getTxHash . HT.txHash $ signedTx
      -- Display transaction id and back button when transaction is sent
      showBackBtnD <- holdDyn False (True <$ broadcastedE)
      void $ networkHoldDynE $ ffor showBackBtnD $ \showBackBtn ->
        if not showBackBtn
          then pure never
          else mdo
            mkrow SSTxId (makeTxIdLink $ HT.txHashToHex . HT.txHash $ signedTx) True
            goE <- divClass "mt-1" $ outlineButton SendBtnBack
            nextWidget $ ffor goE $ const $ Retractable {
                  retractableNext = balancesPage
                , retractablePrev = thisWidget
              }
