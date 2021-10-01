{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Widget.Input.Recipient(
    recipientWidget
  ) where

import Data.Maybe

import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Validate
import Sepulcas.Camera
import Sepulcas.Clipboard
import Sepulcas.Elements
import Sepulcas.Text
import Sepulcas.Validate

import qualified Data.Text as T

recipientWidget :: (MonadFront t m, Display a, Validate a)
  => Currency
  -> Maybe a -- ^ Initial input value (recipient address)
  -> Event t () -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe a))
recipientWidget cur mInitRecipient submitE = divClass "recipient-input" $ mdo
  let initRecipient = maybe "" display mInitRecipient
  recipientErrsD <- holdDyn Nothing $ ffor (current validatedRecipientD `tag` submitE) eitherToMaybe'
  recipientD <- if isAndroid
    then mdo
      (recipD, events) <- validatedTextFieldWithSetValBtns RecipientString initRecipient recipientErrsD ["fas fa-paste", "fas fa-qrcode"] (leftmost [pasteE, resQRcodeE])
      let pasteBtnE = head events
          qrBtnE = events !! 1
          getAddrFromUri t = T.takeWhile (/= '?') $ fromMaybe t $ T.stripPrefix (curprefix cur) t
      openE <- delay 1.0 =<< openCamara qrBtnE
      resQRcodeE <- fmap getAddrFromUri <$> waiterResultCamera openE
      pasteE <- clipboardPaste pasteBtnE
      pure recipD
    else mdo
      (recipD, events) <- validatedTextFieldWithSetValBtns RecipientString initRecipient recipientErrsD ["fas fa-paste"] pasteE
      let pasteBtnE = head events
      pasteE <- clipboardPaste pasteBtnE
      pure recipD
  let validatedRecipientD = toEither . validate <$> recipientD
  pure $ eitherToMaybe <$> validatedRecipientD
