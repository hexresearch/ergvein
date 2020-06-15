module Ergvein.Wallet.Page.Send (
    sendPage
  ) where

import Control.Monad.Except
import Data.Either (isRight)
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Wallet.Camera
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

sendPage :: MonadFront t m => Currency -> Maybe (EgvAddress, Rational) -> m ()
sendPage cur minit = wrapper (SendTitle cur) (Just $ pure $ sendPage cur Nothing) False $ do
  let thisWidget = Just $ pure $ sendPage cur minit
  navbarWidget cur thisWidget NavbarSend
  divClass "centered-wrapper" $ divClass "centered-content" $ divClass "send-page" $ form $ fieldset $ mdo
    recipientErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . fst)
    recipientD <- validatedTextFieldSetVal RecipientString "" recipientErrsD resQRcodeE
    (qrE, pasteE, resQRcodeE) <- divClass "send-buttons-wrapper" $ do
      qrE <- outlineButtonWithIcon BtnScanQRCode "fas fa-qrcode fa-lg"
      openE <- delay 1.0 =<< openCamara qrE
      resQRcodeE <- waiterResultCamera openE
      pasteE <- outlineButtonWithIcon BtnPasteString "fas fa-clipboard fa-lg"
      pure (qrE, pasteE, resQRcodeE)
    amountErrsD <- holdDyn Nothing $ ffor validationE (either Just (const Nothing) . snd)
    amountD <- validatedTextField AmountString "" amountErrsD
    submitE <- submitClass "button button-outline send-submit" SendBtnString
    let validationE = poke submitE $ \_ -> do
          recipient <- sampleDyn recipientD
          amount <- sampleDyn amountD
          pure (V.toEither $ validateRecipient cur (T.unpack recipient),
                V.toEither $ validateAmount $ T.unpack amount)
        goE = fforMaybe validationE $ \(a,b) -> either (const Nothing) Just $ (,) <$> a <*> b
    nextWidget $ ffor goE $ \v -> Retractable {
        retractableNext = feesDispatch v
      , retractablePrev = Just $ pure $ sendPage cur $ Just v
      }
    pure ()

feesDispatch :: MonadFront t m => (EgvAddress, Rational) -> m ()
feesDispatch (adr, rat) = case adr of
  BtcAddress ba -> btcFees ba rat
  ErgAddress ea -> pure ()

btcFees :: MonadFront t m => BtcAddress -> Rational -> m ()
btcFees adr val = pure ()
