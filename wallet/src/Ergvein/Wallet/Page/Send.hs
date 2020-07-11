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
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Validate
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
sendPage cur minit = wrapper False (SendTitle cur) (Just $ pure $ sendPage cur Nothing) $ mdo
  let thisWidget = Just $ sendPage cur <$> retInfoD
  let recipientInit = maybe "" (\(_,_,a) -> egvAddrToString a) minit
  let amountInit = (\(a,_,_) -> a) <$> minit
  let feeInit = (\(_,f,_) -> f) <$> minit
  navbarWidget cur thisWidget NavbarSend
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
  pure ()

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

btcSendConfirmationWidget :: MonadFront t m => ((UnitBTC, Word64), Word64, EgvAddress) -> m ()
btcSendConfirmationWidget v@((unit, amount), fee, addr) = wrapper False (SendTitle BTC) (Just $ pure $ btcSendConfirmationWidget v) $ divClass "send-confirm-box" $ do
  let thisWidget = Just $ pure $ btcSendConfirmationWidget v
  navbarWidget BTC thisWidget NavbarSend
  psD <- getPubStorageD
  utxoKeyD <- holdUniqDyn $ do
    ps <- psD
    let utxo = ps ^. pubStorage'currencyPubStorages . at BTC & fmap (view currencyPubStorage'utxos)
        mkey = getLastUnusedKey Internal =<< pubStorageKeyStorage BTC ps
    pure (utxo, mkey)

  stxE <- fmap switchDyn $ widgetHoldDyn $ ffor utxoKeyD $ \case
    (Nothing, _) -> confirmationErrorWidget CEMEmptyUTXO
    (_, Nothing) -> confirmationErrorWidget CEMNoChangeKey
    (Just utxomap, Just (changeIndex, changeKey)) -> do
      let (confs, unconfs) = partition' $ M.toList utxomap
          firstpick = HT.chooseCoins amount fee 2 True $ L.sort confs
          finalpick = either (const $ HT.chooseCoins amount fee 2 True $ L.sort $ confs <> unconfs) Right firstpick
      either' finalpick (const $ confirmationErrorWidget CEMNoSolution) $ \(pick, change) -> do
        let keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress $ pubKeyBox'key changeKey
        let outs = [(egvAddrToString addr, amount), (keyTxt, change)]
        let etx = HT.buildAddrTx btcNetwork (upPoint <$> pick) outs
        let estFee = HT.guessTxFee fee 2 $ length pick
        confirmationInfoWidget (unit, amount) estFee addr
        either' etx (const $ confirmationErrorWidget CEMTxBuildFail) $ \tx -> do
          signE <- outlineButton ("Sign tx" :: Text)
          etxE <- fmap (fmapMaybe id) $ withWallet $ (signTxWithWallet tx pick) <$ signE
          widgetHold (pure ()) $ ffor etxE $ either (const $ void $ confirmationErrorWidget CEMSignFail) (const $ pure ())
          handleDangerMsg $ (either (Left . T.pack) Right) <$> etxE
  widgetHold (pure ()) $ ffor stxE $ el "h4" . text . (<>) "TxId: " . HT.txHashToHex . HT.txHash

  txD <- holdDyn Nothing $ Just <$> stxE
  addOutgoingTx $ (flip BtcTx Nothing) <$> stxE
  storedE <- btcMempoolTxInserter stxE
  requestBroadcast $ attachWithMaybe (\m _ ->
    fmap (NodeReqBTC . MInv . Inv . pure . InvVector InvTx . HT.getTxHash . HT.txHash) m
    ) (current txD) storedE
  pure ()
  where
    maybe' m n j = maybe n j m
    either' e l r = either l r e
    foo b f ta = L.foldl' f b ta
    partition' :: [(HT.OutPoint, UtxoMeta)] -> ([UtxoPoint], [UtxoPoint])
    partition' = foo ([], []) $ \(cs, ucs) (op, meta@UtxoMeta{..}) ->
      let upoint = UtxoPoint op meta in
      case utxoMeta'status of
        EUtxoConfirmed -> (upoint:cs, ucs)
        EUtxoSemiConfirmed _ -> (upoint:cs, ucs)
        EUtxoSending _ -> (cs, ucs)
        EUtxoReceiving _ -> (cs, upoint:ucs)

-- | TODO: modify to accomodate Ergo
confirmationInfoWidget :: MonadFront t m => (UnitBTC, Word64) -> Word64 -> EgvAddress -> m ()
confirmationInfoWidget (unit, amount) estFee addr = divClass "send-confirm-info" $ do
  h4  $ text "Confirm the transaction"
  mkrow "Send :" $ showMoneyUnit (mkMoney amount) us <> " " <> symbolUnit cur us
  mkrow "To   :" $ egvAddrToString addr
  mkrow "Fee  :" $ showt estFee <> " " <> symbolUnit cur (Units (Just BtcSat) Nothing)
  mkrow "Total:" $ showMoneyUnit (mkMoney $ amount + estFee) us
  where
    cur = egvAddrCurrency addr
    mkMoney = Money cur
    us = Units (Just unit) Nothing
    mkrow :: MonadFront t m => Text -> Text -> m ()
    mkrow a b = divClass "row" $ do
      divClass "col-2 mr-1" $ localizedText a
      divClass "col-10 ta-l ml-1" $ localizedText b

confirmationErrorWidget :: MonadFront t m => ConfirmationErrorMessage -> m (Event t a)
confirmationErrorWidget cem = do
  el "h4" $ localizedText cem
  el "div" $ retract =<< outlineButton ErrBackBtn
  pure never

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

btcFeeSelectionWidget :: forall t m . MonadFront t m
  => Maybe (BTCFeeMode, Word64)
  -> Event t ()
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
