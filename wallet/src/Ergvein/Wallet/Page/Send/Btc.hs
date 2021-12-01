{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Page.Send.Btc (
    sendPageBtc
  ) where

import Control.Lens
import Control.Monad.Except
import Data.Either (fromLeft)
import Data.Maybe
import Data.Tuple.Select
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
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Widget.Input.Amount
import Ergvein.Wallet.Widget.Input.Fee
import Ergvein.Wallet.Widget.Input.Recipient
import Ergvein.Wallet.Wrapper
import Sepulcas.Alert
import Sepulcas.Elements
import Sepulcas.Elements.Toggle
import Sepulcas.Text (Display(..))
import Sepulcas.Validate

import Network.Haskoin.Network (Inv(..), InvVector(..), InvType(..), Message(..))

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.Haskoin.Transaction as HT

sendPageBtc :: MonadFront t m => Maybe ((Word64, UnitBTC), (Word64, FeeMode), BtcAddress, RbfEnabled) -> m ()
sendPageBtc mInit = mdo
  walletName <- getWalletName
  title <- localized walletName
  let navbar = if isAndroid
        then blank
        else navbarWidget BTC thisWidget NavbarSend
      thisWidget = Just $ sendPageBtc <$> retInfoD
  retInfoD <- sendWidget mInit title navbar thisWidget
  pure ()

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

sendWidget :: MonadFront t m
  => Maybe ((Word64, UnitBTC), (Word64, FeeMode), BtcAddress, RbfEnabled)
  -> Dynamic t Text
  -> m a
  -> Maybe (Dynamic t (m ()))
  -> m (Dynamic t (Maybe ((Word64, UnitBTC), (Word64, FeeMode), BtcAddress, RbfEnabled)))
sendWidget mInit title navbar thisWidget = wrapperNavbar False title thisWidget navbar $ divClass "send-page" $ do
  settings <- getSettings
  let mAmountInit = sel1 <$> mInit
      mFeeInit = sel2 <$> mInit
      mRecipientInit = sel3 <$> mInit
      mRbfInit = sel4 <$> mInit
      rbfFromSettings = btcSettings'sendRbfByDefault $ getBtcSettings settings
      rbfInit = fromMaybe rbfFromSettings mRbfInit
  formClass "mb-0" $ mdo
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
      toggler $ pure rbfInit
    submitE <- outlineSubmitTextIconButtonClass "w-100 mb-0" SendBtnString "fas fa-paper-plane fa-lg"
    let
      goE = flip push submitE $ const $ do
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
          (Right recipient, Right amount, Right feeRate) -> pure $ Just ((amount, unit), (feeRate, feeMode), recipient, rbfEnabled)
          _ -> pure Nothing
    void $ nextWidget $ ffor goE $ \v@((amount, unit), (feeRate, _), addr, rbf) -> Retractable {
        retractableNext = sendConfirmationWidget ((amount, unit), feeRate, addr, rbf)
      , retractablePrev = Just $ pure $ sendPageBtc $ Just v
      }
    holdDyn mInit $ Just <$> goE

-- | Main confirmation & sign & send widget
sendConfirmationWidget :: MonadFront t m => ((Word64, UnitBTC), Word64, BtcAddress, RbfEnabled) -> m ()
sendConfirmationWidget v = do
  walletName <- getWalletName
  title <- localized walletName
  let thisWidget = Just $ pure $ sendConfirmationWidget v
  let navbar = if isAndroid
        then blank
        else navbarWidget BTC thisWidget NavbarSend
  wrapperNavbar False title thisWidget navbar $ divClass "send-confirm-box" $ mdo
    stxE <- makeTxWidget v
    void $ networkHold (pure ()) $ ffor stxE $ \tx -> do
      sendE <- getPostBuild
      addedE <- addOutgoingTx "sendConfirmationWidget" $ TxBtc (BtcTx tx Nothing) <$ sendE
      storedE <- btcMempoolTxInserter $ tx <$ addedE
      void $ requestBroadcast $ ffor storedE $ const $
        NodeReqBtc . MInv . Inv . pure . InvVector InvTx . HT.getTxHash . HT.txHash $ tx
      goE <- delay 1 =<< outlineButton SendBtnBack
      void $ nextWidget $ ffor goE $ const $ Retractable {
            retractableNext = balancesPage
          , retractablePrev = thisWidget
        }

makeTxWidget :: MonadFront t m =>
  ((Word64, UnitBTC), Word64, BtcAddress, RbfEnabled) ->
  m (Event t HT.Tx)
makeTxWidget ((amount, unit), fee, addr, rbfEnabled) = mdo
  psD <- getPubStorageD
  utxoKeyD <- holdUniqDyn $ do
    ps <- psD
    let utxo = ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'utxos
        mkey = getLastUnusedKey Internal =<< pubStorageKeyStorage BTC ps
    pure (utxo, mkey)
  utxoKey0 <- Left <$> sampleDyn utxoKeyD -- Why we need this
  stxE' <- eventToNextFrame stxE -- And this
  valD <- foldDynMaybe mergeVals utxoKey0 $ leftmost [Left <$> updated utxoKeyD, Right <$> stxE']
  stxE <- fmap switchDyn $ networkHoldDyn $ ffor valD $ \case
    Left (Nothing, _) -> confirmationErrorWidget CEMEmptyUTXO
    Left (_, Nothing) -> confirmationErrorWidget CEMNoChangeKey
    Left (Just _, Just (_, changeKey)) -> do
      ps <- sampleDyn psD
      let recepientOutputType = btcAddrToBtcOutType addr
          changeOutputType = BtcP2WPKH
          outputTypes = [recepientOutputType, changeOutputType]
          (confs, unconfs) = getBtcUtxoPointsParted ps
          firstpick = chooseCoins amount fee outputTypes Nothing True $ L.sort confs
          finalpick = either (const $ chooseCoins amount fee outputTypes Nothing True $ L.sort $ confs <> unconfs) Right firstpick
      either' finalpick (const $ confirmationErrorWidget CEMNoSolution) $ \(pick, change) ->
        txSignSendWidget addr unit amount fee changeKey change pick rbfEnabled
    Right (tx, unit', amount', estFee, addr') -> do
      confirmationInfoWidget (unit', amount') estFee rbfEnabled addr' (Just tx)
      pure never
  pure $ (\(tx, _, _, _, _) -> tx) <$> stxE
  where
    either' e l r = either l r e
    -- Left -- utxo updates, Right -- stored tx
    mergeVals newval origval = case (newval, origval) of
      (Left a, Left _)    -> Just $ Left a
      (Left _, Right _)   -> Nothing
      (Right a, Left _)   -> Just $ Right a
      (Right _, Right _)  -> Nothing

-- | Simply displays the relevant information about a transaction
confirmationInfoWidget :: MonadFront t m => (UnitBTC, Word64) -> Word64 -> RbfEnabled -> BtcAddress -> Maybe HT.Tx -> m ()
confirmationInfoWidget (unit, amount) estFee rbfEnabled addr mTx = divClass "send-confirm-info ta-l mb-1" $ do
  elClass "h4" "ta-c mb-1" $ localizedText $
    if isJust mTx then SSPosted else SSConfirm
  mkrow AmountString (text $ showMoneyUnit (mkMoney amount) unit <> " " <> display unit) False
  mkrow RecipientString (text $ btcAddrToText addr) True
  mkrow SSFee (text $ showt estFee <> " " <> display BtcSat) False
  mkrow SSRbf (localizedText $ FSRbf rbfEnabled) False
  mkrow SSTotal (text $ showMoneyUnit (mkMoney $ amount + estFee) unit <> " " <> display unit) False
  case mTx of
    Nothing -> pure ()
    Just tx -> mkrow SSTxId (makeTxIdLink $ HT.txHashToHex . HT.txHash $ tx) True
  where
    cur = BTC
    mkMoney = Money cur

    mkrow :: (MonadFront t m, LocalizedPrint l) => l -> m b -> Bool -> m ()
    mkrow a mb wordBreak = divClass "" $ do
      elClass "span" "font-bold" $ do
        localizedText a
        text ": "
      let wordBreakClass = if wordBreak then "word-break-all" else ""
      void $ elClass "span" wordBreakClass mb

    makeTxIdLink :: MonadFront t m => Text -> m ()
    makeTxIdLink txIdText = do
      settings <- getSettings
      let urlPrefixes = btcSettings'explorerUrls $ getBtcSettings settings
          urlPrefix = if isTestnet then testnetUrl urlPrefixes else mainnetUrl urlPrefixes
      hyperlink "link" txIdText (urlPrefix <> "/tx/" <> txIdText)

-- | A handy patch to display various errors
confirmationErrorWidget :: MonadFront t m => ConfirmationErrorMessage -> m (Event t a)
confirmationErrorWidget cem = do
  el "h4" $ localizedText cem
  void $ retract =<< outlineButton SendBtnBack
  pure never

-- | This widget builds & signs the transaction
txSignSendWidget :: MonadFront t m
  => BtcAddress     -- ^ The recipient
  -> UnitBTC        -- ^ BTC Unit to send
  -> Word64         -- ^ Amount of BTC in the units
  -> Word64         -- ^ Fee rate in sat/vbyte
  -> EgvPubKeyBox   -- ^ Keybox to send the change to
  -> Word64         -- ^ Change
  -> [UtxoPoint]    -- ^ List of utxo points used as inputs
  -> RbfEnabled     -- ^ Explicit opt-in RBF signalling
  -> m (Event t (HT.Tx, UnitBTC, Word64, Word64, BtcAddress)) -- ^ Return the Tx + all relevant information for display
txSignSendWidget addr unit amount _ changeKey change pick rbfEnabled = mdo
  let keyTxt = btcAddrToText $ xPubToBtcAddr $ extractXPubKeyFromEgv $ pubKeyBox'key changeKey
      outs = [(btcAddrToText addr, amount), (keyTxt, change)]
      etx = buildAddrTx btcNetwork rbfEnabled (upPoint <$> pick) outs
      inputsAmount = sum $ btcUtxo'amount . upMeta <$> pick
      outputsAmount = amount + change
      estFee = inputsAmount - outputsAmount
  confirmationInfoWidget (unit, amount) estFee rbfEnabled addr Nothing
  showSignD <- holdDyn True . (False <$) =<< eventToNextFrame etxE
  etxE <- either' etx (const $ confirmationErrorWidget CEMTxBuildFail >> pure never) $ \tx -> do
    fmap switchDyn $ networkHoldDyn $ ffor showSignD $ \b -> if not b then pure never else do
      signE <- outlineButton SendBtnSign
      etxE' <- fmap (fmapMaybe id) $ withWallet $ signTxWithWallet tx pick <$ signE
      void $ networkHold (pure ()) $ ffor etxE' $ either (const $ void $ confirmationErrorWidget CEMSignFail) (const $ pure ())
      handleDangerMsg $ either (Left . T.pack) Right <$> etxE'
  fmap switchDyn $ networkHold (pure never) $ ffor etxE $ \tx -> do
    sendE <- el "div" $ outlineButton SendBtnSend
    pure $ (tx, unit, amount, estFee, addr) <$ sendE
  where either' e l r = either l r e
