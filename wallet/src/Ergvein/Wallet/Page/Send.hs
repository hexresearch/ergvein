module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

data SendTitle = SendTitle !Currency

instance LocalizedPrint SendTitle where
  localizedShow l v = case l of
    English -> case v of
      SendTitle c -> "Send " <> currencyName c
    Russian -> case v of
      SendTitle c -> "Отправить " <> currencyName c

data SendBtnString = SendBtnString

instance LocalizedPrint SendBtnString where
  localizedShow l _ = case l of
    English -> "Send"
    Russian -> "Отправить"

data RecipientString = RecipientString

instance LocalizedPrint RecipientString where
  localizedShow l _ = case l of
    English -> "Recipient"
    Russian -> "Получатель"

data AmountString = AmountString

instance LocalizedPrint AmountString where
  localizedShow l _ = case l of
    English -> "Amount"
    Russian -> "Сумма"

data BtnPasteString = BtnPasteString

instance LocalizedPrint BtnPasteString where
  localizedShow l _ = case l of
    English -> "Paste"
    Russian -> "Вставить"

data BtnScanQRCode = BtnScanQRCode

instance LocalizedPrint BtnScanQRCode where
  localizedShow l _ = case l of
    English -> "Scan"
    Russian -> "Сканировать"

sendPage :: MonadFront t m => Currency -> m ()
sendPage cur = do
  let thisWidget = Just $ pure $ sendPage cur
  menuWidget (SendTitle cur) thisWidget
  wrapper True $ divClass "send-wrapper" $ do
    recipientE <- textField RecipientString ""
    divClass "send-buttons-wrapper" $ do
      qrE <- outlineButtonWithIcon BtnScanQRCode "fas fa-qrcode fa-lg"
      pasteE <- outlineButtonWithIcon BtnPasteString "fas fa-clipboard fa-lg"
      pure()
    amountE <- textField AmountString ""
    submitE <- submitClass "button button-outline send-submit" SendBtnString
    pure ()
