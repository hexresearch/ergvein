{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

module Ergvein.Wallet.Widget.Input.BTC.Recipient(
    recipientWidget
  ) where

import Data.Maybe

import Ergvein.Either
import Ergvein.Types
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Elements.Input
import Ergvein.Wallet.Localization.Send
import Ergvein.Wallet.Localization.Settings()
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Validate

#ifdef ANDROID
import Ergvein.Wallet.Camera
#endif

import qualified Data.Text as T

recipientWidget :: MonadFront t m
  => Maybe BtcAddress -- ^ Initial input value
  -> Event t () -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe BtcAddress))
recipientWidget mInitRecipient submitE = mdo
  let initRecipient = maybe "" btcAddrToString mInitRecipient
  recipientErrsD <- holdDyn Nothing $ ffor (current validatedRecipientD `tag` submitE) eitherToMaybe'
  recipientD <- if isAndroid
    then mdo
      recipD <- validatedTextFieldSetVal RecipientString initRecipient recipientErrsD (leftmost [resQRcodeE, pasteE])
      (pasteE, resQRcodeE) <- divClass "send-page-buttons-wrapper" $ do
        qrE <- outlineTextIconButtonTypeButton CSScanQR "fas fa-qrcode fa-lg"
        openE <- delay 1.0 =<< openCamara qrE
        let stripCurPrefix t = T.dropWhile (== '/') $ fromMaybe t $ T.stripPrefix (curprefix BTC) t
        resQRcodeE' <- (fmap . fmap) stripCurPrefix $ waiterResultCamera openE
        pasteBtnE <- outlineTextIconButtonTypeButton CSPaste "fas fa-clipboard fa-lg"
        pasteE' <- clipboardPaste pasteBtnE
        pure (pasteE', resQRcodeE')
      pure recipD
    else mdo
      recipD <- validatedTextFieldSetVal RecipientString initRecipient recipientErrsD pasteE
      pasteE <- divClass "send-page-buttons-wrapper" $ do
        clipboardPaste =<< outlineTextIconButtonTypeButton CSPaste "fas fa-clipboard fa-lg"
      pure recipD
  let validatedRecipientD = toEither . validateBtcRecipient . T.unpack <$> recipientD
  pure $ eitherToMaybe <$> validatedRecipientD
