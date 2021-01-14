{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Control.Lens
import Control.Monad.Except
import Data.Maybe
import Data.Word
import Text.Read

import Ergvein.Text
import Ergvein.Types
import Ergvein.Types.Derive
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Elements.Input
import Ergvein.Wallet.Elements.Toggle
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Send
import Ergvein.Wallet.Localization.Settings()
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Node
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Transaction.Util
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Wrapper

import Data.Validation (toEither)
import Network.Haskoin.Network (Inv(..), InvVector(..), InvType(..), Message(..))

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Transaction as HT

#ifdef ANDROID
import Ergvein.Wallet.Camera
#endif

type RbfEnabled = Bool

sendPage :: MonadFront t m => Currency -> Maybe ((UnitBTC, Word64), (BTCFeeMode, Word64), EgvAddress, RbfEnabled) -> m ()
sendPage cur minit = mdo
  walletName <- getWalletName
  title <- localized walletName
  let navbar = if isAndroid
        then blank
        else navbarWidget cur thisWidget NavbarSend
      thisWidget = Just $ sendPage cur <$> retInfoD
  retInfoD <- sendWidget title navbar thisWidget
  pure ()
  where
    stripCurPrefix t = T.dropWhile (== '/') $ fromMaybe t $ T.stripPrefix (curprefix cur) t
    -- TODO: write type annotation here
    sendWidget title navbar thisWidget = wrapperNavbar False title thisWidget navbar $ mdo
      settings <- getSettings
      let recipientInit = maybe "" (\(_, _, a, _) -> egvAddrToString a) minit
          amountInit = (\(am, _, _, _) -> am) <$> minit
          feeInit = (\(_, f, _, _) -> f) <$> minit
          rbfInit = (\(_, _, _, r) -> r) <$> minit
          rbfFromSettings = btcSettings'sendRbfByDefault $ getBtcSettings settings
          rbfInit' = fromMaybe rbfFromSettings rbfInit
      retInfoD <- form $ mdo
        recipientErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing))
        recipientD <- if isAndroid
          then mdo
            recipD <- validatedTextFieldSetVal RecipientString recipientInit recipientErrsD (leftmost [resQRcodeE, pasteE])
            (pasteE, resQRcodeE) <- divClass "send-page-buttons-wrapper" $ do
              qrE <- outlineTextIconButtonTypeButton CSScanQR "fas fa-qrcode fa-lg"
              openE <- delay 1.0 =<< openCamara qrE
              resQRcodeE' <- (fmap . fmap) stripCurPrefix $ waiterResultCamera openE
              pasteBtnE <- outlineTextIconButtonTypeButton CSPaste "fas fa-clipboard fa-lg"
              pasteE' <- clipboardPaste pasteBtnE
              pure (pasteE', resQRcodeE')
            pure recipD
          else mdo
            recipD <- validatedTextFieldSetVal RecipientString recipientInit recipientErrsD pasteE
            pasteE <- divClass "send-page-buttons-wrapper" $ do
              clipboardPaste =<< outlineTextIconButtonTypeButton CSPaste "fas fa-clipboard fa-lg"
            pure recipD
        amountD <- sendAmountWidget amountInit $ () <$ validationE
        feeD <- btcFeeSelectionWidget feeInit submitE
        rbfInitD <- holdDyn rbfInit' never
        rbfEnabledD <- divClass "mb-1" $ labeledToggler SSRbf rbfInitD
        submitE <- outlineSubmitTextIconButtonClass "w-100" SendBtnString "fas fa-paper-plane fa-lg"
        let validationE = poke submitE $ \_ -> do
              recipient <- sampleDyn recipientD
              pure (toEither $ validateRecipient cur (T.unpack $ stripCurPrefix recipient))
            goE = flip push validationE $ \erecipient -> do
              mfee <- sampleDyn feeD
              mamount <- sampleDyn amountD
              rbfEnabled <- sampleDyn rbfEnabledD
              let mrecipient = either (const Nothing) Just erecipient
              pure $ (,,,) <$> mamount <*> mfee <*> mrecipient <*> (Just rbfEnabled)
        void $ nextWidget $ ffor goE $ \v@(uam, (_, fee), addr, rbf) -> Retractable {
            retractableNext = btcSendConfirmationWidget (uam, fee, addr, rbf)
          , retractablePrev = Just $ pure $ sendPage cur $ Just v
          }
        holdDyn minit $ Just <$> goE
      pure retInfoD

data FeeSelectorStatus = FSSNoEntry | FSSNoCache | FSSNoManual | FSSManual Word64 | FSSLvl (FeeLevel, Word64)

data UtxoPoint = UtxoPoint {
  upPoint :: !HT.OutPoint
, upMeta  :: !BtcUtxoMeta
} deriving (Show)

instance Eq UtxoPoint where
  a == b = (btcUtxo'amount $ upMeta a) == (btcUtxo'amount $ upMeta b)

-- | We need to sort in desc order to reduce Tx size
instance Ord UtxoPoint where
  a `compare` b = (btcUtxo'amount $ upMeta b) `compare` (btcUtxo'amount $ upMeta a)

instance HT.Coin UtxoPoint where
  coinValue = btcUtxo'amount . upMeta

-- | Main confirmation & sign & send widget
btcSendConfirmationWidget :: MonadFront t m => ((UnitBTC, Word64), Word64, EgvAddress, RbfEnabled) -> m ()
btcSendConfirmationWidget v@((unit, amount), fee, addr, rbfEnabled) = do
  walletName <- getWalletName
  title <- localized walletName
  let thisWidget = Just $ pure $ btcSendConfirmationWidget v
      navbar = if isAndroid
        then blank
        else navbarWidget BTC thisWidget NavbarSend
  wrapperNavbar False title thisWidget navbar $ divClass "send-confirm-box" $ mdo
    psD <- getPubStorageD
    utxoKeyD <- holdUniqDyn $ do
      ps <- psD
      let utxo = ps ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'utxos
          mkey = getLastUnusedKey Internal =<< pubStorageKeyStorage BTC ps
      pure $ (utxo, mkey)
    utxoKey0 <- fmap Left $ sampleDyn utxoKeyD
    stxE' <- eventToNextFrame stxE
    valD <- foldDynMaybe mergeVals utxoKey0 $ leftmost [Left <$> (updated utxoKeyD), Right <$> stxE']
    stxE <- fmap switchDyn $ widgetHoldDyn $ ffor valD $ \case
      Left (Nothing, _) -> confirmationErrorWidget CEMEmptyUTXO
      Left (_, Nothing) -> confirmationErrorWidget CEMNoChangeKey
      Left (Just utxomap, Just (_, changeKey)) -> do
        let (confs, unconfs) = partition' $ M.toList utxomap
            firstpick = HT.chooseCoins amount fee 2 True $ L.sort confs
            finalpick = either (const $ HT.chooseCoins amount fee 2 True $ L.sort $ confs <> unconfs) Right firstpick
        either' finalpick (const $ confirmationErrorWidget CEMNoSolution) $ \(pick, change) ->
          txSignSendWidget addr unit amount fee changeKey change pick rbfEnabled
      Right (tx, unit', amount', estFee, addr') -> do
        confirmationInfoWidget (unit', amount') estFee addr' (Just tx)
        pure never
    void $ widgetHold (pure ()) $ ffor stxE $ \(tx, _, _, _, _) -> do
      sendE <- getPostBuild
      addedE <- addOutgoingTx "btcSendConfirmationWidget" $ (TxBtc $ BtcTx tx Nothing) <$ sendE
      storedE <- btcMempoolTxInserter $ tx <$ addedE
      void $ requestBroadcast $ ffor storedE $ const $
        NodeReqBTC . MInv . Inv . pure . InvVector InvTx . HT.getTxHash . HT.txHash $ tx
      goE <- delay 1 =<< outlineButton SendBtnBack
      void $ nextWidget $ ffor goE $ const $ Retractable {
            retractableNext = balancesPage
          , retractablePrev = Nothing
        }
  where
    either' e l r = either l r e
    foo b f ta = L.foldl' f b ta
    -- Left -- utxo updates, Right -- stored tx
    mergeVals newval origval = case (newval, origval) of
      (Left a, Left _)    -> Just $ Left a
      (Left _, Right _)   -> Nothing
      (Right a, Left _)   -> Just $ Right a
      (Right _, Right _)  -> Nothing

    -- | Split utxo set into confirmed and unconfirmed points
    partition' :: [(HT.OutPoint, BtcUtxoMeta)] -> ([UtxoPoint], [UtxoPoint])
    partition' = foo ([], []) $ \(cs, ucs) (opoint, meta@BtcUtxoMeta{..}) ->
      let upoint = UtxoPoint opoint meta in
      case btcUtxo'status of
        EUtxoConfirmed -> (upoint:cs, ucs)
        EUtxoSemiConfirmed _ -> (upoint:cs, ucs)
        EUtxoSending _ -> (cs, ucs)
        EUtxoReceiving _ -> (cs, upoint:ucs)

-- | Simply displays the relevant information about a transaction
-- TODO: modify to accomodate Ergo
confirmationInfoWidget :: MonadFront t m => (UnitBTC, Word64) -> Word64 -> EgvAddress -> Maybe HT.Tx -> m ()
confirmationInfoWidget (unit, amount) estFee addr mTx = divClass "send-confirm-info ta-l mb-1" $ do
  elClass "h4" "ta-c mb-1" $ localizedText $
    if isJust mTx then SSPosted else SSConfirm
  mkrow AmountString (text $ showMoneyUnit (mkMoney amount) us <> " " <> symbolUnit cur us) False
  mkrow RecipientString (text $ egvAddrToString addr) True
  mkrow SSFee (text $ showt estFee <> " " <> symbolUnit cur (Units (Just BtcSat) Nothing)) False
  mkrow SSTotal (text $ showMoneyUnit (mkMoney $ amount + estFee) us <> " " <> symbolUnit cur us) False
  case mTx of
    Nothing -> pure ()
    Just tx -> mkrow SSTxId (makeTxIdLink $ HT.txHashToHex . HT.txHash $ tx) True
  where
    cur = egvAddrCurrency addr
    mkMoney = Money cur
    us = Units (Just unit) Nothing

    mkrow :: (MonadFront t m, LocalizedPrint l) => l -> m b -> Bool -> m ()
    mkrow a mb wordBreak = divClass "" $ do
      elClass "span" "font-bold" $ do
        localizedText a
        text ": "
      let wordBreakClass = if wordBreak then "word-break-all" else ""
      void $ elClass "span" wordBreakClass $ mb

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
  => EgvAddress     -- ^ The recipient
  -> UnitBTC        -- ^ BTC Unit to send
  -> Word64         -- ^ Amount of BTC in the units
  -> Word64         -- ^ Fee rate satoshi/vbyte
  -> EgvPubKeyBox   -- ^ Keybox to send the change to
  -> Word64         -- ^ Change
  -> [UtxoPoint]    -- ^ List of utxo points used as inputs
  -> RbfEnabled
  -> m (Event t (HT.Tx, UnitBTC, Word64, Word64, EgvAddress)) -- ^ Return the Tx + all relevant information for display
txSignSendWidget addr unit amount fee changeKey change pick rbfEnabled = mdo
  let keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress $ pubKeyBox'key changeKey
  let outs = [(egvAddrToString addr, amount), (keyTxt, change)]
  let etx = if rbfEnabled
        then buildAddrTxRbf btcNetwork (upPoint <$> pick) outs
        else HT.buildAddrTx btcNetwork (upPoint <$> pick) outs
  let estFee = HT.guessTxFee fee 2 $ length pick
  confirmationInfoWidget (unit, amount) estFee addr Nothing
  showSignD <- holdDyn True . (False <$) =<< eventToNextFrame etxE
  etxE <- either' etx (const $ confirmationErrorWidget CEMTxBuildFail >> pure never) $ \tx -> do
    fmap switchDyn $ widgetHoldDyn $ ffor showSignD $ \b -> if not b then pure never else do
      signE <- outlineButton SendBtnSign
      etxE' <- fmap (fmapMaybe id) $ withWallet $ (signTxWithWallet tx pick) <$ signE
      void $ widgetHold (pure ()) $ ffor etxE' $ either (const $ void $ confirmationErrorWidget CEMSignFail) (const $ pure ())
      handleDangerMsg $ (either (Left . T.pack) Right) <$> etxE'
  fmap switchDyn $ widgetHold (pure never) $ ffor etxE $ \tx -> do
    sendE <- el "div" $ outlineButton SendBtnSend
    pure $ (tx, unit, amount, estFee, addr) <$ sendE
  where either' e l r = either l r e

-- | Sign function which has access to the private storage
-- TODO: generate missing private keys
signTxWithWallet :: (MonadIO m, PlatformNatives) => HT.Tx -> [UtxoPoint] -> PrvStorage -> m (Maybe (Either String HT.Tx))
signTxWithWallet tx pick prv = do
  let PrvKeystore _ ext int = prv ^. prvStorage'currencyPrvStorages
        . at BTC . non (error "btcSendConfirmationWidget: not exsisting store!")
        . currencyPrvStorage'prvKeystore
  mvals <- fmap (fmap unzip . sequence) $ flip traverse pick $ \(UtxoPoint opoint BtcUtxoMeta{..}) -> do
    let sig = HT.SigInput btcUtxo'script btcUtxo'amount opoint HS.sigHashAll Nothing
    let errMsg = "Failed to get a corresponding secret key: " <> showt btcUtxo'purpose <> " #" <> showt btcUtxo'index
    let msec = case btcUtxo'purpose of
          Internal -> fmap (xPrvKey . unEgvXPrvKey) $ (V.!?) int btcUtxo'index
          External -> fmap (xPrvKey . unEgvXPrvKey) $ (V.!?) ext btcUtxo'index
    maybe (logWrite errMsg >> pure Nothing) (pure . Just . (sig,)) msec
  pure $ (uncurry $ HT.signTx btcNetwork tx) <$> mvals

-- | Btc fee selector
btcFeeSelectionWidget :: forall t m . MonadFront t m
  => Maybe (BTCFeeMode, Word64)                 -- ^ Inital mode and value
  -> Event t ()                                 -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe (BTCFeeMode, Word64)))
btcFeeSelectionWidget minit sendE = do
  feesD <- getFeesD
  divClass "fee-widget" $ do
    el "label" $ localizedText FSLevel
    statD <- el "div" $ mdo
      let lvlE = leftmost [lowE, midE, highE, manE]
      lvlD <- holdDyn (fst <$> minit) lvlE
      mmanD <- holdDyn (maybe "" (showt . snd) minit) $ "" <$ lvlE
      let attrD m = ffor lvlD $ \m' -> if m' == Just m
            then "button button-outline btn-fee btn-fee-on"
            else "button button-outline btn-fee"
      lowE  <- fmap (Just BFMLow <$)    $ buttonClass (attrD BFMLow)    BFMLow
      midE  <- fmap (Just BFMMid <$)    $ buttonClass (attrD BFMMid)    BFMMid
      highE <- fmap (Just BFMHigh <$)   $ buttonClass (attrD BFMHigh)   BFMHigh
      manE  <- fmap (Just BFMManual <$) $ buttonClass (attrD BFMManual) BFMManual
      fmap join $ widgetHoldDyn $ ffor lvlD $ \case
        Nothing         -> pure (pure FSSNoEntry)
        Just BFMManual  -> manualFeeSelector =<< sampleDyn mmanD
        Just BFMLow     -> pure $ extractFeeD feesD FeeCheap
        Just BFMMid     -> pure $ extractFeeD feesD FeeModerate
        Just BFMHigh    -> pure $ extractFeeD feesD FeeFast
    attrD <- holdDyn [] $ leftmost [[("class", "lbl-red")] <$ sendE, [] <$ updated statD]
    divClass "fee-descr" $ el "label" $ widgetHoldDyn $ ffor statD $ \case
      FSSNoEntry    -> elDynAttr "label" attrD (localizedText FSSelect)   >> pure Nothing
      FSSNoCache    -> elDynAttr "label" attrD (localizedText FSNoFees)   >> pure Nothing
      FSSNoManual   -> elDynAttr "label" attrD (localizedText FSInvalid)  >> pure Nothing
      FSSManual f   -> el "label" (localizedText $ FSFee f)               >> pure (Just (BFMManual, f))
      FSSLvl (l, f) -> el "label" (localizedText $ FSLevelDesc l f)       >> pure (Just $ (feeLvlToMode l, f))
  where
    extractFeeD feesD lvl = ffor feesD $
      maybe FSSNoCache (FSSLvl . (lvl,) . fromIntegral . fst . extractFee lvl) . M.lookup BTC
    feeLvlToMode lvl = case lvl of
      FeeCheap    -> BFMLow
      FeeModerate -> BFMMid
      FeeFast     -> BFMHigh

manualFeeSelector :: MonadFront t m => Text -> m (Dynamic t FeeSelectorStatus)
manualFeeSelector = (fmap . fmap) (maybe FSSNoManual FSSManual . readMaybe . T.unpack) . el "div" . textFieldNoLabel

-- | Input field with units. Converts everything to satoshis and returns the unit
sendAmountWidget :: MonadFront t m => Maybe (UnitBTC, Word64) -> Event t () -> m (Dynamic t (Maybe (UnitBTC, Word64)))
sendAmountWidget minit validateE = mdo
  setUs <- fmap (fromMaybe defUnits . _settingsUnits) getSettings
  let (unitInit, txtInit) = maybe (setUs, "") (\(u, a) -> let us = Units (Just u) Nothing
        in (us, showMoneyUnit (Money BTC a) us)) minit
  let errsD = fmap (maybe [] id) amountErrsD
  let isInvalidD = fmap (maybe "" (const "is-invalid")) amountErrsD
  amountValD <- el "div" $ mdo
    textInputValueD <- (fmap . fmap) T.unpack $ divClassDyn isInvalidD $ textField AmountString txtInit
    when isAndroid (availableBalanceWidget unitD)
    unitD <- unitsDropdown (getUnitBTC unitInit) allUnitsBTC
    pure $ zipDynWith (\u v -> fmap (u,) $ toEither $ validateBtcWithUnits u v) unitD textInputValueD
  void $ divClass "form-field-errors" $ simpleList errsD displayError
  amountErrsD <- holdDyn Nothing $ ffor (current amountValD `tag` validateE) (either Just (const Nothing))
  pure $ (either (const Nothing) Just) <$> amountValD
  where
    availableBalanceWidget uD = do
      balanceValue <- balancesWidget BTC
      balanceText <- localized SendAvailableBalance
      let balanceVal = zipDynWith (\x y -> showMoneyUnit x (Units (Just y) Nothing) <> " " <> btcSymbolUnit y) balanceValue uD
          balanceTxt = zipDynWith (\x y -> x <> ": " <> y) balanceText balanceVal
      divClass "send-page-available-balance" $ dynText balanceTxt
    unitsDropdown val allUnits = do
      langD <- getLanguage
      let unitD = constDyn val
      initKey <- sample . current $ unitD
      let listUnitsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allUnits
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated unitD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- dropdown initKey listUnitsD ddnCfg
      let selD = _dropdown_value dp
      holdUniqDyn selD
    displayError :: (MonadFrontBase t m, LocalizedPrint l) => Dynamic t l -> m ()
    displayError errD = do
      langD <- getLanguage
      let localizedErrD = zipDynWith localizedShow langD errD
      dynText localizedErrD
      br
