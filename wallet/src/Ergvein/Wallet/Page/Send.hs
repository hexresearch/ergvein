module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Control.Monad.Except
import Ergvein.Text
import Ergvein.Types.Currency
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

data SendTitle = SendTitle !Currency

instance LocalizedPrint SendTitle where
  localizedShow l (SendTitle c) = case l of
    English -> "Send " <> currencyName c
    Russian -> "Отправить " <> currencyName c

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
  navbarWidget cur thisWidget NavbarSend
  wrapper True $ divClass "send-page" $ form $ fieldset $ mdo
    recipientErrsD <- holdDyn Nothing recipientErrsE
    recipientD <- validatedTextField RecipientString "" recipientErrsD
    (qrE, pasteE) <- divClass "send-buttons-wrapper" $ do
      qrE <- outlineButtonWithIcon BtnScanQRCode "fas fa-qrcode fa-lg"
      pasteE <- outlineButtonWithIcon BtnPasteString "fas fa-clipboard fa-lg"
      pure (qrE, pasteE)
    amountErrsD <- holdDyn Nothing amountErrsE
    amountD <- validatedTextField AmountString "" amountErrsD
    submitE <- submitClass "button button-outline send-submit" SendBtnString
    let recipientErrsE = poke submitE $ \_ -> do
          recipient <- sampleDyn recipientD
          case validateNonEmptyString $ T.unpack recipient of
            Failure errs -> pure $ Just errs
            Success _ -> pure Nothing
    let amountErrsE = poke submitE $ \_ -> do
          amount <- sampleDyn amountD
          case (\x -> validateNonEmptyString x *> validateRational x) $ T.unpack amount of
            Failure errs -> pure $ Just errs
            Success _ -> pure Nothing
    pure ()
