module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Control.Monad.Except
import Data.Either (isRight)
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
import qualified Data.Validation as V

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
sendPage currency = do
  let thisWidget = Just $ pure $ sendPage currency
  menuWidget (SendTitle currency) thisWidget
  navbarWidget currency thisWidget NavbarSend
  wrapper True $ divClass "send-page" $ form $ fieldset $ mdo
    recipientErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . fst)
    recipientD <- validatedTextField RecipientString "" recipientErrsD
    (qrE, pasteE) <- divClass "send-buttons-wrapper" $ do
      qrE <- outlineButtonWithIcon BtnScanQRCode "fas fa-qrcode fa-lg"
      pasteE <- outlineButtonWithIcon BtnPasteString "fas fa-clipboard fa-lg"
      pure (qrE, pasteE)
    amountErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . snd)
    amountD <- validatedTextField AmountString "" amountErrsD
    submitE <- submitClass "button button-outline send-submit" SendBtnString
    let validationE = poke submitE $ \_ -> do
          recipient <- sampleDyn recipientD
          amount <- sampleDyn amountD
          pure (V.toEither $ validateRecipient currency (T.unpack recipient),
                V.toEither $ validateAmount $ T.unpack amount)
        validatedE = traceEvent "valid" $ fforMaybe validationE (\x -> if (isRight $ fst x) && (isRight $ snd x) then Just x else Nothing) 
    pure ()
