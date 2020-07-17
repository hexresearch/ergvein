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
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Send
import Ergvein.Wallet.Localization.Settings()
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Node
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Storage.Keys
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

sendPage :: MonadFront t m => Currency -> Maybe ((UnitBTC, Word64), (BTCFeeMode, Word64), EgvAddress) -> m ()
sendPage cur minit = mdo
  title <- balanceTitleWidget cur
  let navbar = navbarWidget cur thisWidget NavbarSend
      thisWidget = Just $ sendPage cur <$> retInfoD
  retInfoD <- sendWidget cur minit title navbar thisWidget
  pure ()
  where
    -- TODO: write type annotation here
    sendWidget cur minit title navbar thisWidget = wrapperNavbar False title thisWidget navbar $ mdo
      let recipientInit = maybe "" (\(_,_,a) -> egvAddrToString a) minit
          amountInit = (\(a,_,_) -> a) <$> minit
          feeInit = (\(_,f,_) -> f) <$> minit
      retInfoD <- form $ mdo
        recipientErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing))
#ifdef ANDROID
        recipientD <- validatedTextFieldSetVal RecipientString recipientInit recipientErrsD (leftmost [resQRcodeE, pasteE])
        (qrE, pasteE, resQRcodeE) <- divClass "send-page-buttons-wrapper" $ do
          qrE <- outlineTextIconButtonTypeButton BtnScanQRCode "fas fa-qrcode fa-lg"
          openE <- delay 1.0 =<< openCamara qrE
          resQRcodeE <- waiterResultCamera openE
          pasteBtnE <- outlineTextIconButtonTypeButton BtnPasteString "fas fa-clipboard fa-lg"
          pasteE <- clipboardPaste pasteBtnE
          pure (qrE, pasteE, resQRcodeE)
#else
        recipientD <- validatedTextFieldSetVal RecipientString recipientInit recipientErrsD pasteE
        pasteE <- divClass "send-page-buttons-wrapper" $ do
          clipboardPaste =<< outlineTextIconButtonTypeButton BtnPasteString "fas fa-clipboard fa-lg"
#endif
        amountD <- sendAmountWidget amountInit $ () <$ validationE
        feeD    <- btcFeeSelectionWidget feeInit submitE
        submitE <- outlineSubmitTextIconButtonClass "w-100" SendBtnString "fas fa-paper-plane fa-lg"
        let validationE = poke submitE $ \_ -> do
              recipient <- sampleDyn recipientD
              pure (toEither $ validateRecipient cur (T.unpack recipient))
            goE = flip push validationE $ \erecipient -> do
              mfee <- sampleDyn feeD
              mamount <- sampleDyn amountD
              let mrecipient = either (const Nothing) Just erecipient
              pure $ (,,) <$> mamount <*> mfee <*> mrecipient
        nextWidget $ ffor goE $ \v@(uam, (m, fee), addr) -> Retractable {
            retractableNext = btcSendConfirmationWidget (uam, fee, addr)
          , retractablePrev = Just $ pure $ sendPage cur $ Just v
          }
        holdDyn minit $ Just <$> goE
      pure retInfoD

data FeeSelectorStatus = FSSNoEntry | FSSNoCache | FSSNoManual | FSSManual Word64 | FSSLvl (FeeLevel, Word64)

data UtxoPoint = UtxoPoint {
  upPoint :: !HT.OutPoint
, upMeta  :: !UtxoMeta
} deriving (Show)

instance Eq UtxoPoint where
  a == b = (utxoMeta'amount $ upMeta a) == (utxoMeta'amount $ upMeta b)

-- | We need to sort in desc order to reduce Tx size
instance Ord UtxoPoint where
  a `compare` b = (utxoMeta'amount $ upMeta b) `compare` (utxoMeta'amount $ upMeta a)

instance HT.Coin UtxoPoint where
  coinValue = utxoMeta'amount . upMeta

-- | Main confirmation & sign & send widget
btcSendConfirmationWidget :: MonadFront t m => ((UnitBTC, Word64), Word64, EgvAddress) -> m ()
btcSendConfirmationWidget v@((unit, amount), fee, addr) = do
  title <- balanceTitleWidget BTC
  let thisWidget = Just $ pure $ btcSendConfirmationWidget v
      navbar = navbarWidget BTC thisWidget NavbarSend
  wrapperNavbar False title thisWidget navbar $ divClass "send-confirm-box" $ mdo
    psD <- getPubStorageD
    utxoKeyD <- holdUniqDyn $ do
      ps <- psD
      let utxo = ps ^. pubStorage'currencyPubStorages . at BTC & fmap (view currencyPubStorage'utxos)
          mkey = getLastUnusedKey Internal =<< pubStorageKeyStorage BTC ps
      pure $ (utxo, mkey)
    utxoKey0 <- fmap Left $ sampleDyn utxoKeyD
    stxE' <- eventToNextFrame stxE
    valD <- foldDynMaybe mergeVals utxoKey0 $ leftmost [Left <$> (updated utxoKeyD), Right <$> stxE']
    stxE <- fmap switchDyn $ widgetHoldDyn $ ffor valD $ \case
      Left (Nothing, _) -> confirmationErrorWidget CEMEmptyUTXO
      Left (_, Nothing) -> confirmationErrorWidget CEMNoChangeKey
      Left (Just utxomap, Just (changeIndex, changeKey)) -> do
        let (confs, unconfs) = partition' $ M.toList utxomap
            firstpick = HT.chooseCoins amount fee 2 True $ L.sort confs
            finalpick = either (const $ HT.chooseCoins amount fee 2 True $ L.sort $ confs <> unconfs) Right firstpick
        either' finalpick (const $ confirmationErrorWidget CEMNoSolution) $ \(pick, change) ->
          txSignSendWidget addr unit amount fee changeKey change pick
      Right (tx, unit, amount, estFee, addr) -> do
        confirmationInfoWidget (unit, amount) estFee addr
        el "h4" . text . (<>) "TxId: " . HT.txHashToHex . HT.txHash $ tx
        pure never
    widgetHold (pure ()) $ ffor stxE $ \(tx, _, _, _, _) -> do
      sendE <- getPostBuild
      addedE <- addOutgoingTx "btcSendConfirmationWidget" $ (BtcTx tx Nothing) <$ sendE
      storedE <- btcMempoolTxInserter $ tx <$ addedE
      reqE <- requestBroadcast $ ffor storedE $ const $
        NodeReqBTC . MInv . Inv . pure . InvVector InvTx . HT.getTxHash . HT.txHash $ tx
      goE <- el "div" $ delay 1 =<< outlineButton SendBtnBack
      void $ nextWidget $ ffor goE $ const $ Retractable {
            retractableNext = balancesPage
          , retractablePrev = Nothing
        }
    pure ()
  where
    maybe' m n j = maybe n j m
    either' e l r = either l r e
    foo b f ta = L.foldl' f b ta
    -- Left -- utxo updates, Right -- stored tx
    mergeVals newval origval = case (newval, origval) of
      (Left a, Left _)    -> Just $ Left a
      (Left _, Right _)   -> Nothing
      (Right a, Left _)   -> Just $ Right a
      (Right _, Right _)  -> Nothing

    -- | Split utxo set into confirmed and unconfirmed points
    partition' :: [(HT.OutPoint, UtxoMeta)] -> ([UtxoPoint], [UtxoPoint])
    partition' = foo ([], []) $ \(cs, ucs) (op, meta@UtxoMeta{..}) ->
      let upoint = UtxoPoint op meta in
      case utxoMeta'status of
        EUtxoConfirmed -> (upoint:cs, ucs)
        EUtxoSemiConfirmed _ -> (upoint:cs, ucs)
        EUtxoSending _ -> (cs, ucs)
        EUtxoReceiving _ -> (cs, upoint:ucs)

-- | Simply displays the relevant information about a transaction
-- TODO: modify to accomodate Ergo
confirmationInfoWidget :: MonadFront t m => (UnitBTC, Word64) -> Word64 -> EgvAddress -> m ()
confirmationInfoWidget (unit, amount) estFee addr = divClass "send-confirm-info" $ do
  h4  $ localizedText SSConfirm
  divClass "mb-1 ml-1 mr-1" $ do
    mkrow AmountString $ showMoneyUnit (mkMoney amount) us <> " " <> symbolUnit cur us
    mkrow RecipientString $ egvAddrToString addr
    mkrow SSFee $ showt estFee <> " " <> symbolUnit cur (Units (Just BtcSat) Nothing)
    mkrow SSTotal $ showMoneyUnit (mkMoney $ amount + estFee) us <> " " <> symbolUnit cur us
  where
    cur = egvAddrCurrency addr
    mkMoney = Money cur
    us = Units (Just unit) Nothing
    mkrow :: (MonadFront t m, LocalizedPrint l) => l -> Text -> m ()
    mkrow a b = divClass "ta-l" $ do
      lD <- getLanguage
      elClass "span" "font-bold" $ dynText $ do
        l <- lD
        pure $ localizedShow l a <> ":"
      elClass "span" "word-break-all ml-1" $ text b

-- | A handy patch to display various errors
confirmationErrorWidget :: MonadFront t m => ConfirmationErrorMessage -> m (Event t a)
confirmationErrorWidget cem = do
  el "h4" $ localizedText cem
  el "div" $ retract =<< outlineButton SendBtnBack
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
  -> m (Event t (HT.Tx, UnitBTC, Word64, Word64, EgvAddress)) -- ^ Return the Tx + all relevant information for display
txSignSendWidget addr unit amount fee changeKey change pick = mdo
  let keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress $ pubKeyBox'key changeKey
  let outs = [(egvAddrToString addr, amount), (keyTxt, change)]
  let etx = HT.buildAddrTx btcNetwork (upPoint <$> pick) outs
  let estFee = HT.guessTxFee fee 2 $ length pick
  confirmationInfoWidget (unit, amount) estFee addr
  showSignD <- holdDyn True . (False <$) =<< eventToNextFrame etxE
  etxE <- either' etx (const $ confirmationErrorWidget CEMTxBuildFail >> pure never) $ \tx -> do
    fmap switchDyn $ widgetHoldDyn $ ffor showSignD $ \b -> if not b then pure never else do
      signE <- outlineButton SendBtnSign
      etxE <- fmap (fmapMaybe id) $ withWallet $ (signTxWithWallet tx pick) <$ signE
      widgetHold (pure ()) $ ffor etxE $ either (const $ void $ confirmationErrorWidget CEMSignFail) (const $ pure ())
      handleDangerMsg $ (either (Left . T.pack) Right) <$> etxE
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
  mvals <- fmap (fmap unzip . sequence) $ flip traverse pick $ \(UtxoPoint op UtxoMeta{..}) -> do
    let sig = HT.SigInput utxoMeta'script utxoMeta'amount op HS.sigHashAll Nothing
    let errMsg = "Failed to get a corresponding secret key: " <> showt utxoMeta'purpose <> " #" <> showt utxoMeta'index
    let msec = case utxoMeta'purpose of
          Internal -> fmap (xPrvKey . unEgvXPrvKey) $ (V.!?) int utxoMeta'index
          External -> fmap (xPrvKey . unEgvXPrvKey) $ (V.!?) ext utxoMeta'index
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
  setUs <- fmap (fromMaybe defUnits . settingsUnits) getSettings
  let (unitInit, txtInit) = maybe (setUs, "") (\(u, a) -> let us = Units (Just u) Nothing
        in (us, showMoneyUnit (Money BTC a) us)) minit
  let errsD = fmap (maybe [] id) amountErrsD
  let isInvalidD = fmap (maybe "" (const "is-invalid")) amountErrsD
  amountValD <- el "div" $ do
    textInputValueD <- (fmap . fmap) T.unpack $ divClassDyn isInvalidD $ textField AmountString txtInit
    unitD <- unitsDropdown (getUnitBTC unitInit) allUnitsBTC
    pure $ zipDynWith (\u v -> fmap (u,) $ toEither $ validateBtcWithUnits u v) unitD textInputValueD
  divClass "form-field-errors" $ simpleList errsD displayError
  amountErrsD <- holdDyn Nothing $ ffor (current amountValD `tag` validateE) (either Just (const Nothing))
  pure $ (either (const Nothing) Just) <$> amountValD
  where
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
