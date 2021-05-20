{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Control.Lens
import Control.Monad.Except
import Data.Maybe
import Data.Word

import Ergvein.Text
import Ergvein.Types
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Utxo.Btc
import Sepulcas.Alert
import Sepulcas.Elements
import Sepulcas.Elements.Toggle
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Widget.Input.BTC.Amount
import Ergvein.Wallet.Widget.Input.BTC.Fee
import Ergvein.Wallet.Widget.Input.BTC.Recipient
import Ergvein.Wallet.Wrapper

import Network.Haskoin.Network (Inv(..), InvVector(..), InvType(..), Message(..))

import qualified Data.List as L
import qualified Data.Text as T
import qualified Network.Haskoin.Address as HA
import qualified Network.Haskoin.Transaction as HT

sendPage :: MonadFront t m => Currency -> Maybe ((UnitBTC, Word64), (BTCFeeMode, Word64), BtcAddress, RbfEnabled) -> m ()
sendPage cur mInit = mdo
  walletName <- getWalletName
  title <- localized walletName
  let navbar = if isAndroid
        then blank
        else navbarWidget cur thisWidget NavbarSend
      thisWidget = Just $ sendPage cur <$> retInfoD
  retInfoD <- sendWidget cur mInit title navbar thisWidget
  pure ()

sendWidget :: MonadFront t m
  => Currency
  -> Maybe ((UnitBTC, Word64), (BTCFeeMode, Word64), BtcAddress, RbfEnabled)
  -> Dynamic t Text
  -> m a
  -> Maybe (Dynamic t (m ()))
  -> m (Dynamic t (Maybe ((UnitBTC, Word64), (BTCFeeMode, Word64), BtcAddress, RbfEnabled)))
sendWidget cur mInit title navbar thisWidget = wrapperNavbar False title thisWidget navbar $ divClass "send-page" $ mdo
  settings <- getSettings
  let amountInit = (\(x, _, _, _) -> x) <$> mInit
      feeInit = (\(_, x, _, _) -> x) <$> mInit
      recipientInit = (\(_, _, x, _) -> x) <$> mInit
      rbfInit = (\(_, _, _, x) -> x) <$> mInit
      rbfFromSettings = btcSettings'sendRbfByDefault $ getBtcSettings settings
      rbfInit' = fromMaybe rbfFromSettings rbfInit
  retInfoD <- formClass "mb-0" $ mdo
    recipientD <- divClass "mb-1" $ recipientWidget recipientInit submitE
    amountD <- divClass "mb-1" $ sendAmountWidget amountInit submitE
    feeD <- divClass "mb-1" $ btcFeeSelectionWidget FSRate feeInit Nothing submitE
    rbfEnabledD <- divClass "mb-2" $ toggler SSRbf (constDyn rbfInit')
    submitE <- outlineSubmitTextIconButtonClass "w-100 mb-0" SendBtnString "fas fa-paper-plane fa-lg"
    let goE = flip push submitE $ \_ -> do
          mrecipient <- sampleDyn recipientD
          mamount <- sampleDyn amountD
          mfee <- sampleDyn feeD
          rbfEnabled <- sampleDyn rbfEnabledD
          pure $ (,,,) <$> mamount <*> mfee <*> mrecipient <*> (Just rbfEnabled)
    void $ nextWidget $ ffor goE $ \v@(uam, (_, fee), addr, rbf) -> Retractable {
        retractableNext = btcSendConfirmationWidget (uam, fee, addr, rbf)
      , retractablePrev = Just $ pure $ sendPage cur $ Just v
      }
    holdDyn mInit $ Just <$> goE
  pure retInfoD

-- | Main confirmation & sign & send widget
btcSendConfirmationWidget :: MonadFront t m => ((UnitBTC, Word64), Word64, BtcAddress, RbfEnabled) -> m ()
btcSendConfirmationWidget v = do
  walletName <- getWalletName
  title <- localized walletName
  let thisWidget = Just $ pure $ btcSendConfirmationWidget v
      navbar = if isAndroid
        then blank
        else navbarWidget BTC thisWidget NavbarSend
  wrapperNavbar False title thisWidget navbar $ divClass "send-confirm-box" $ mdo
    stxE <- makeTxWidget v
    void $ networkHold (pure ()) $ ffor stxE $ \(tx, _, _, _, _) -> do
      sendE <- getPostBuild
      addedE <- addOutgoingTx "btcSendConfirmationWidget" $ (TxBtc $ BtcTx tx Nothing) <$ sendE
      storedE <- btcMempoolTxInserter $ tx <$ addedE
      void $ requestBroadcast $ ffor storedE $ const $
        NodeReqBtc . MInv . Inv . pure . InvVector InvTx . HT.getTxHash . HT.txHash $ tx
      goE <- delay 1 =<< outlineButton SendBtnBack
      void $ nextWidget $ ffor goE $ const $ Retractable {
            retractableNext = balancesPage
          , retractablePrev = Nothing
        }

btcAddrToBtcOutType :: BtcAddress -> BtcAddressType
btcAddrToBtcOutType = \case
  HA.PubKeyAddress _ -> BtcP2PKH
  HA.ScriptAddress _ -> BtcP2SH
  HA.WitnessPubKeyAddress _ -> BtcP2WPKH
  HA.WitnessScriptAddress _ -> BtcP2WSH

makeTxWidget :: MonadFront t m =>
  ((UnitBTC, Word64), Word64, BtcAddress, RbfEnabled) ->
  m (Event t (HT.Tx, UnitBTC, Word64, Word64, BtcAddress))
makeTxWidget ((unit, amount), fee, addr, rbfEnabled) = mdo
  psD <- getPubStorageD
  utxoKeyD <- holdUniqDyn $ do
    ps <- psD
    let utxo = ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'utxos
        mkey = getLastUnusedKey Internal =<< pubStorageKeyStorage BTC ps
    pure $ (utxo, mkey)
  utxoKey0 <- fmap Left $ sampleDyn utxoKeyD -- Why we need this
  stxE' <- eventToNextFrame stxE -- And this
  valD <- foldDynMaybe mergeVals utxoKey0 $ leftmost [Left <$> (updated utxoKeyD), Right <$> stxE']
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
  pure stxE
  where
    either' e l r = either l r e
    -- Left -- utxo updates, Right -- stored tx
    mergeVals newval origval = case (newval, origval) of
      (Left a, Left _)    -> Just $ Left a
      (Left _, Right _)   -> Nothing
      (Right a, Left _)   -> Just $ Right a
      (Right _, Right _)  -> Nothing

-- | Simply displays the relevant information about a transaction
-- TODO: modify to accomodate Ergo
confirmationInfoWidget :: MonadFront t m => (UnitBTC, Word64) -> Word64 -> RbfEnabled -> BtcAddress -> Maybe HT.Tx -> m ()
confirmationInfoWidget (unit, amount) estFee rbfEnabled addr mTx = divClass "send-confirm-info ta-l mb-1" $ do
  elClass "h4" "ta-c mb-1" $ localizedText $
    if isJust mTx then SSPosted else SSConfirm
  mkrow AmountString (text $ showMoneyUnit (mkMoney amount) us <> " " <> symbolUnit cur us) False
  mkrow RecipientString (text $ btcAddrToString addr) True
  mkrow SSFee (text $ showt estFee <> " " <> symbolUnit cur (Units (Just BtcSat) Nothing)) False
  mkrow SSRbf (localizedText $ FSRbf rbfEnabled) False
  mkrow SSTotal (text $ showMoneyUnit (mkMoney $ amount + estFee) us <> " " <> symbolUnit cur us) False
  case mTx of
    Nothing -> pure ()
    Just tx -> mkrow SSTxId (makeTxIdLink $ HT.txHashToHex . HT.txHash $ tx) True
  where
    cur = BTC
    mkMoney = Money cur
    us = Units (Just unit) Nothing

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
  let keyTxt = btcAddrToString $ xPubToBtcAddr $ extractXPubKeyFromEgv $ pubKeyBox'key changeKey
      outs = [(btcAddrToString addr, amount), (keyTxt, change)]
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
