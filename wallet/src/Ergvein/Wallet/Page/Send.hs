{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Control.Monad.Except
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
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper

import qualified Data.Text as T
import qualified Data.Validation as V
import qualified Data.Map.Strict as M

data SendStrings
  = SendTitle Currency
  | SendBtnString
  | RecipientString
  | AmountString
  | BtnPasteString
  | BtnScanQRCode

instance LocalizedPrint SendStrings where
  localizedShow l v = case l of
    English -> case v of
      SendTitle c -> "Send " <> currencyName c
      SendBtnString -> "Send"
      RecipientString -> "Recipient"
      AmountString -> "Amount"
      BtnPasteString -> "Paste"
      BtnScanQRCode -> "Scan"
    Russian -> case v of
      SendTitle c -> "Отправить " <> currencyName c
      SendBtnString -> "Отправить"
      RecipientString -> "Получатель"
      AmountString -> "Сумма"
      BtnPasteString -> "Вставить"
      BtnScanQRCode -> "Сканировать"

sendPage :: MonadFront t m => Currency -> Maybe (EgvAddress, Rational) -> m ()
sendPage cur minit = wrapper False (SendTitle cur) (Just $ pure $ sendPage cur Nothing) $ do
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
    pure ()

data BTCFeeMode = BFMLow | BFMMid | BFMHigh | BFMManual
  deriving (Eq)

instance LocalizedPrint BTCFeeMode where
  localizedShow l v = case l of
    English -> case v of
      BFMLow    -> "Low"
      BFMMid    -> "Mid"
      BFMHigh   -> "High"
      BFMManual -> "Manual"
    Russian -> case v of
      BFMLow    -> "Низкий"
      BFMMid    -> "Средний"
      BFMHigh   -> "Высокий"
      BFMManual -> "Вручную"

data FeeStrings
  = FSLevel
  | FSSelect
  | FSLevelDesc (FeeLevel, Int)
  | FSFee Int
  | FSInvalid
  | FSNoFees

instance LocalizedPrint FeeStrings where
  localizedShow l v = case l of
    English -> case v of
      FSLevel -> "Fee level"
      FSSelect -> "Select fee level"
      FSLevelDesc (lvl,f) -> "~" <> showt f <> " satoshi/vbyte. <" <> showt (feeTargetBlocks BTC lvl) <> " blocks."
      FSFee f -> "~" <> showt f <> " satoshi/vbyte"
      FSInvalid -> "Enter valid integer fee in satoshi/vbyte"
      FSNoFees -> "Fees not found in the cache. Please enter the fee manually."
    Russian -> case v of
      FSLevel -> "Уровень комиссии"
      FSSelect -> "Выберите уровень комиссии"
      FSLevelDesc (lvl,f) -> "~" <> showt f <> " satoshi/vbyte. <" <> showt (feeTargetBlocks BTC lvl) <> " блоков."
      FSFee f -> "~" <> showt f <> " satoshi/vbyte"
      FSInvalid -> "Введите комиссию. Целое число, satoshi/vbyte"
      FSNoFees -> "Уровень комиссий не найден в кэше. Пожалуйста, введите комиссию вручную."

data FeeSelectorStatus = FSSNoEntry | FSSNoCache | FSSNoManual | FSSManual Int | FSSLvl (FeeLevel, Int)

btcFeeSelectionWidget :: forall t m . MonadFront t m => Event t () -> m (Dynamic t (Maybe Int))
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
