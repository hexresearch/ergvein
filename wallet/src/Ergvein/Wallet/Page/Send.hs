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
import qualified Data.Vector as Ve

import Control.Lens
import Ergvein.Types.Storage
import Ergvein.Types.Utxo
import Data.Maybe
import Ergvein.Wallet.Storage.Keys
import Ergvein.Types.Keys
import Network.Haskoin.Constants
import Ergvein.Wallet.Platform

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

data UtxoPoint = UtxoPoint {
  upIndex :: !Int
, upPoint :: !HT.OutPoint
, upValue :: !Word64
} deriving (Show)

instance Eq UtxoPoint where
  a == b = upValue a == upValue b

-- | We need to sort in desc order to reduce Tx size
instance Ord UtxoPoint where
  a `compare` b = upValue b `compare` upValue a

instance HT.Coin UtxoPoint where
  coinValue = upValue

btcSendConfirmationWidget :: MonadFront t m => (Word64, Word64, EgvAddress) -> m ()
btcSendConfirmationWidget v@(amount, fee, addr) = wrapper False (SendTitle BTC) (Just $ pure $ btcSendConfirmationWidget v) $ do
  let thisWidget = Just $ pure $ btcSendConfirmationWidget v
  navbarWidget BTC thisWidget NavbarSend
  el "div" $ text $ showt v
  psD <- getPubStorageD
  widgetHoldDyn $ ffor psD $ \ps -> do
    let utxomap = fromMaybe M.empty $ ps ^. pubStorage'currencyPubStorages . at BTC & fmap (view currencyPubStorage'utxos)
        (confs, unconfs) = partition' $ M.toList utxomap
        firstpick = HT.chooseCoins amount fee 2 True $ L.sort confs
        finalpick = case firstpick of
          Right v -> Right v
          Left err -> HT.chooseCoins amount fee 2 True $ L.sort $ confs <> unconfs
        mkey = getLastUnusedKey Internal =<< pubStorageKeyStorage BTC ps
        keys = fromMaybe Ve.empty $ fmap pubKeystore'internal $ pubStorageKeyStorage BTC ps
    case finalpick of
      Left _ -> el "div" $ text $ showt "No solution found. Not enough money"
      Right (pick, change) -> do
        let n = length pick
        let ops = upPoint <$> pick
        let estFee = HT.guessTxFee fee 2 n
        el "div" $ text $ "Fee estimate: " <> showt estFee
        el "div" $ text $ "Change: " <> showt change
        void $ traverse (el "div" . text . showt) ops
        el "div" $ text $ maybe "No key" (showt . fmap (addrToString btcTest . xPubToBtcAddr . extractXPubKeyFromEgv . pubKeyBox'key)) mkey
        el "div" $ text $ "--------------------------------------------------------------------------"
        case mkey of
          Nothing -> el "div" $ text "Something bad happend. No key -- no Tx"
          Just (_, key) -> do
            let keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress $ pubKeyBox'key key
            -- flip traverse (Ve.indexed keys) $ el "div" . text . showt . fmap (addrToString btcTest . xPubToBtcAddr . extractXPubKeyFromEgv . pubKeyBox'key)
            let outs = [(egvAddrToString addr, amount), (keyTxt, change)]
            let etx = HT.buildAddrTx btcNetwork (upPoint <$> pick) outs
            case etx of
              Left err -> el "div" $ text $ showt err
              Right tx -> el "div" $ text $ showt tx
        pure ()
    pure ()

  pure ()
  where
    foo b f ta = L.foldl' f b ta
    partition' :: [(HT.OutPoint, UtxoMeta)] -> ([UtxoPoint], [UtxoPoint])
    partition' = foo ([], []) $ \(cs, ucs) (op, UtxoMeta{..}) ->
      let upoint = UtxoPoint utxoMeta'index op utxoMeta'amount in
      case utxoMeta'status of
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
