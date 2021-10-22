{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Widget.Input.Recipient(
    recipientWidget
  ) where

import Data.Either (fromLeft)
import Data.Maybe

import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Validate
import Sepulcas.Camera
import Sepulcas.Clipboard
import Sepulcas.Elements
import Sepulcas.Text
import Sepulcas.Validate

import qualified Data.Map.Strict as M
import qualified Data.Text as T

recipientWidget :: (MonadFront t m, Display a, Validate a)
  => Currency
  -> Maybe a -- ^ Initial input value (recipient address)
  -> Event t () -- ^ Send event. Triggers fileds validation
  -> m (Dynamic t (Maybe a))
recipientWidget cur mInitRecipient submitE = divClass "recipient-input" $ mdo
  let initRecipient = maybe "" display mInitRecipient
  -- Be careful with recipientErrsD, it must be declared before labeledTextFieldWithBtns.
  -- Otherwise you can get deadlock.
  recipientErrsD <- holdDyn [] $ ffor (current validatedRecipientD `tag` submitE) (fromLeft [])
  recipientD <- if isAndroid
    then mdo
      (recipD, events) <- labeledTextFieldWithBtns RecipientString initRecipient M.empty [mkIconBtn "fas fa-paste", mkIconBtn "fas fa-qrcode"] (pasteE <> resQRcodeE) never recipientErrsD
      let pasteBtnE = head events
          qrBtnE = events !! 1
          getAddrFromUri t = T.takeWhile (/= '?') $ fromMaybe t $ T.stripPrefix (curprefix cur) t
      openE <- delay 1.0 =<< openCamara qrBtnE
      resQRcodeE <- fmap getAddrFromUri <$> waiterResultCamera openE
      pasteE <- clipboardPaste pasteBtnE
      pure recipD
    else mdo
      (recipD, events) <- labeledTextFieldWithBtns RecipientString initRecipient M.empty [mkIconBtn "fas fa-paste"] pasteE never recipientErrsD
      let pasteBtnE = head events
      pasteE <- clipboardPaste pasteBtnE
      pure recipD
  let validatedRecipientD = toEither . validate <$> recipientD
  pure $ eitherToMaybe <$> validatedRecipientD
  where mkIconBtn iconClass = elClass "i" iconClass blank
