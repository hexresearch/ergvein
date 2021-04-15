{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

module Ergvein.Wallet.Widget.Input.BTC.Recipient(
    recipientWidget
  ) where

import Data.Maybe

import Ergvein.Either
import Ergvein.Types
import Sepulcas.Camera
import Sepulcas.Clipboard
import Sepulcas.Elements
import Ergvein.Wallet.Localization.Send
import Ergvein.Wallet.Localization.Settings()
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Validate

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
      (recipD, events) <- validatedTextFieldWithSetValBtns RecipientString initRecipient recipientErrsD ["fas fa-paste", "fas fa-qrcode"] (leftmost [resQRcodeE, pasteE])
      let pasteBtnE = head events
          qrBtnE = events !! 1
          stripCurPrefix t = T.dropWhile (== '/') $ fromMaybe t $ T.stripPrefix (curprefix BTC) t
      openE <- delay 1.0 =<< openCamara qrBtnE
      resQRcodeE <- (fmap . fmap) stripCurPrefix $ waiterResultCamera openE
      pasteE <- clipboardPaste pasteBtnE
      pure recipD
    else mdo
      (recipD, events) <- validatedTextFieldWithSetValBtns RecipientString initRecipient recipientErrsD ["fas fa-paste"] pasteE
      let pasteBtnE = head events
      pasteE <- clipboardPaste pasteBtnE
      pure recipD
  let validatedRecipientD = toEither . validateBtcRecipient . T.unpack <$> recipientD
  pure $ eitherToMaybe <$> validatedRecipientD
