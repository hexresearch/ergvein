{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Control.Monad.Except
import Data.Ratio ((%))
import Data.Word
import Network.Haskoin.Address
import Text.Read

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Send
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Validation as V
import qualified Network.Haskoin.Transaction as HT

import Control.Lens
import Ergvein.Types.Storage
import Ergvein.Types.Utxo
import Data.Maybe

sendPage :: MonadFront t m => Currency -> Maybe (EgvAddress, Rational) -> m ()
sendPage cur minit = do
  buildE <- delay 0.05 =<< getPostBuild
  void $ nextWidget $ ffor buildE $ const $ Retractable {
    retractableNext = btcSendConfirmationWidget debugVals
  , retractablePrev = Nothing
  }

sendPage' :: MonadFront t m => Currency -> Maybe (EgvAddress, Rational) -> m ()
sendPage' cur minit = wrapper False (SendTitle cur) (Just $ pure $ sendPage cur Nothing) $ do
  let thisWidget = Just $ pure $ sendPage cur minit
  navbarWidget cur thisWidget NavbarSend
  form $ mdo
    recipientErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . fst)
#ifdef ANDROID
    recipientD <- validatedTextFieldSetVal RecipientString "" recipientErrsD (leftmost [resQRcodeE, pasteE])
    (qrE, pasteE, resQRcodeE) <- divClass "send-page-buttons-wrapper" $ do
      qrE <- outlineTextIconButtonTypeButton BtnScanQRCode "fas fa-qrcode fa-lg"
      openE <- delay 1.0 =<< openCamara qrE
      resQRcodeE <- waiterResultCamera openE
      pasteBtnE <- outlineTextIconButtonTypeButton BtnPasteString "fas fa-clipboard fa-lg"
      pasteE <- clipboardPaste pasteBtnE
      pure (qrE, pasteE, resQRcodeE)
#else
    recipientD <- validatedTextFieldSetVal RecipientString "" recipientErrsD pasteE
    pasteE <- divClass "send-page-buttons-wrapper" $ do
      pasteBtnE <- outlineTextIconButtonTypeButton BtnPasteString "fas fa-clipboard fa-lg"
      pasteE <- clipboardPaste pasteBtnE
      pure pasteE
#endif
    amountErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . snd)
    amountD <- validatedTextField AmountString "" amountErrsD
    feeD <- btcFeeSelectionWidget submitE
    submitE <- outlineSubmitTextIconButtonClass "w-100" SendBtnString "fas fa-paper-plane fa-lg"
    let validationE = poke submitE $ \_ -> do
          recipient <- sampleDyn recipientD
          amount <- sampleDyn amountD
          pure (V.toEither $ validateRecipient cur (T.unpack recipient),
                V.toEither $ validateAmount $ T.unpack amount)
        goE = flip push validationE $ \(a,b) -> do
          mfee <- sampleDyn feeD
          pure $ join $ ffor mfee $ \fee -> either (const Nothing) Just $ (fee,,) <$> a <*> b
    pure ()

data FeeSelectorStatus = FSSNoEntry | FSSNoCache | FSSNoManual | FSSManual Word64 | FSSLvl (FeeLevel, Word64)

debugVals :: (Word64, Word64, EgvAddress)
debugVals = (1000, 1, BtcAddress {getBtcAddr = WitnessPubKeyAddress {getAddrHash160 = "50beb79f500060a3faaf466d388732c0ebecc6f8"}})

data UtxoPoint = UtxoPoint {upPoint :: !HT.OutPoint, upValue :: !Word64}
  deriving (Show)

instance Eq UtxoPoint where
  (UtxoPoint _ a) == (UtxoPoint _ b) = a == b

-- | We need to sort in desc order to reduce Tx size
instance Ord UtxoPoint where
  (UtxoPoint _ a) `compare` (UtxoPoint _ b) = b `compare` a

instance HT.Coin UtxoPoint where
  coinValue (UtxoPoint _ v) = v

btcSendConfirmationWidget :: MonadFront t m => (Word64, Word64, EgvAddress) -> m ()
btcSendConfirmationWidget v@(amount, fee, addr) = wrapper False (SendTitle BTC) (Just $ pure $ btcSendConfirmationWidget v) $ do
  let thisWidget = Just $ pure $ btcSendConfirmationWidget v
  navbarWidget BTC thisWidget NavbarSend
  el "div" $ text $ showt v
  psD <- getPubStorageD
  widgetHoldDyn $ ffor psD $ \ps -> do
    let utxomap = fromMaybe M.empty $ join $ ps ^. pubStorage'currencyPubStorages . at BTC
          & fmap (getBtcUtxoSetFromStore . view currencyPubStorage'utxos)
        (confs, unconfs) = partition' $ M.toList utxomap
        firstpick = HT.chooseCoins amount fee 2 True $ L.sort confs
        finalpick = case firstpick of
          Right v -> Right v
          Left err -> HT.chooseCoins amount fee 2 True $ L.sort $ confs <> unconfs
    case finalpick of
      Left _ -> el "div" $ text $ showt "No solution found. Not enough money"
      Right (pick, change) -> do
        let n = length pick
        let ops = upPoint <$> pick
        let estFee = HT.guessTxFee fee 2 n
        el "div" $ text $ "Fee estimate: " <> showt estFee
        el "div" $ text $ "Change: " <> showt change
        void $ traverse (el "div" . text . showt) ops

    pure ()

  pure ()
  where
    foo b f ta = L.foldl' f b ta
    partition' :: [(HT.OutPoint, (Word64, EgvUtxoStatus))] -> ([UtxoPoint], [UtxoPoint])
    partition' = foo ([], []) $ \(cs, ucs) (op, (v, stat)) -> let upoint = UtxoPoint op v in case stat of
      EUtxoConfirmed -> (upoint:cs, ucs)
      EUtxoSemiConfirmed _ -> (upoint:cs, ucs)
      EUtxoSending -> (cs, ucs)
      EUtxoReceiving -> (cs, upoint:ucs)

btcFeeSelectionWidget :: forall t m . MonadFront t m => Event t () -> m (Dynamic t (Maybe Word64))
btcFeeSelectionWidget sendE = do
  feesD <- getFeesD
  divClass "fee-widget" $ do
    el "label" $ localizedText FSLevel
    statD <- el "div" $ mdo
      let lvlE = leftmost [lowE, midE, highE, manE]
      lvlD <- holdDyn Nothing lvlE
      let attrD m = ffor lvlD $ \m' -> if m' == Just m
            then "button button-outline btn-fee btn-fee-on"
            else "button button-outline btn-fee"
      lowE  <- fmap (Just BFMLow <$)    $ buttonClass (attrD BFMLow)    BFMLow
      midE  <- fmap (Just BFMMid <$)    $ buttonClass (attrD BFMMid)    BFMMid
      highE <- fmap (Just BFMHigh <$)   $ buttonClass (attrD BFMHigh)   BFMHigh
      manE  <- fmap (Just BFMManual <$) $ buttonClass (attrD BFMManual) BFMManual
      fmap join $ widgetHoldDyn $ ffor lvlD $ \case
        Nothing         -> pure (pure FSSNoEntry)
        Just BFMManual  -> manualFeeSelector
        Just BFMLow     -> pure $ extractFeeD feesD FeeCheap
        Just BFMMid     -> pure $ extractFeeD feesD FeeModerate
        Just BFMHigh    -> pure $ extractFeeD feesD FeeFast
    attrD <- holdDyn [] $ leftmost [[("class", "lbl-red")] <$ sendE, [] <$ updated statD]
    divClass "fee-descr" $ el "label" $ widgetHoldDyn $ ffor statD $ \case
      FSSNoEntry  -> elDynAttr "label" attrD (localizedText FSSelect)   >> pure Nothing
      FSSNoCache  -> elDynAttr "label" attrD (localizedText FSNoFees)   >> pure Nothing
      FSSNoManual -> elDynAttr "label" attrD (localizedText FSInvalid)  >> pure Nothing
      FSSManual f -> el "label" (localizedText $ FSFee f)               >> pure (Just f)
      FSSLvl lf   -> el "label" (localizedText $ FSLevelDesc lf)        >> pure (Just $ snd lf)
  where
    extractFeeD feesD lvl = ffor feesD $
      maybe FSSNoCache (FSSLvl . (lvl,) . fromIntegral . fst . extractFee lvl) . M.lookup BTC

manualFeeSelector :: MonadFront t m => m (Dynamic t FeeSelectorStatus)
manualFeeSelector = (fmap . fmap) (maybe FSSNoManual FSSManual . readMaybe . T.unpack) $ el "div" $ textFieldNoLabel ""
