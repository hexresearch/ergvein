{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Control.Monad.Except
import Data.Either (isRight)
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper

import qualified Data.Text as T
import qualified Data.Validation as V

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

sendPage :: MonadFront t m => Currency -> m ()
sendPage cur = wrapper (SendTitle cur) (Just $ pure $ sendPage cur) WrapperAlignmentNone $ do
  let thisWidget = Just $ pure $ sendPage cur
  navbarWidget cur thisWidget NavbarSend
  contentWrapper WrapperAlignmentVertical WrapperNoPadding $ formClass "send-page" $ mdo
    recipientErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . fst)
#ifdef ANDROID
    recipientD <- validatedTextFieldSetVal RecipientString "" recipientErrsD (leftmost [resQRcodeE, pasteE])
    (qrE, pasteE, resQRcodeE) <- divClass "send-page-buttons-wrapper" $ do
      qrE <- outlineTextIconButton BtnScanQRCode "fas fa-qrcode fa-lg"
      openE <- delay 1.0 =<< openCamara qrE
      resQRcodeE <- waiterResultCamera openE
      pasteBtnE <- outlineTextIconButton BtnPasteString "fas fa-clipboard fa-lg"
      pasteE <- clipboardPaste pasteBtnE
      pure (qrE, pasteE, resQRcodeE)
#else
    recipientD <- validatedTextFieldSetVal RecipientString "" recipientErrsD pasteE
    pasteE <- divClass "send-page-buttons-wrapper" $ do
      pasteBtnE <- outlineTextIconButton BtnPasteString "fas fa-clipboard fa-lg"
      pasteE <- clipboardPaste pasteBtnE
      pure pasteE
#endif
    amountErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . snd)
    amountD <- validatedTextField AmountString "" amountErrsD
    submitE <- outlineSubmitTextIconButtonClass "w-100" SendBtnString "fas fa-paper-plane fa-lg"
    let validationE = poke submitE $ \_ -> do
          recipient <- sampleDyn recipientD
          amount <- sampleDyn amountD
          pure (V.toEither $ validateRecipient cur (T.unpack recipient),
                V.toEither $ validateAmount $ T.unpack amount)
        validatedE = fforMaybe validationE (\x -> if (isRight $ fst x) && (isRight $ snd x) then Just x else Nothing)
    pure ()
