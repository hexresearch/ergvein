{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Widget.Input.Recipient(
    recipientWidget
  ) where

import Data.Maybe

import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Sepulcas.Camera
import Sepulcas.Clipboard
import Sepulcas.Elements
import Sepulcas.Text
import Sepulcas.Validate

import qualified Data.Map.Strict as M
import qualified Data.Text as T

recipientWidget :: (MonadFront t m, LocalizedPrint l, Display a, Validate a)
  => Currency      -- ^ Currency
  -> Maybe a       -- ^ Initial input value (recipient address)
  -> Dynamic t [l] -- ^ Dynamic with errors
  -> m (Dynamic t Text, Event t ())
recipientWidget cur mInitRecipient errsD = divClass "recipient-input" $ mdo
  let initRecipient = maybe "" display mInitRecipient
  if isAndroid
    then mdo
      (recipientD, events) <- labeledTextFieldWithBtns RecipientString initRecipient M.empty [mkIconBtn "fas fa-paste", mkIconBtn "fas fa-qrcode"] setValE never errsD
      let pasteBtnE = head events
          qrBtnE = events !! 1
          getAddrFromUri t = T.takeWhile (/= '?') $ fromMaybe t $ T.stripPrefix (curprefix cur) t
          setValE = leftmost [pasteE, resQRcodeE]
      openE <- delay 1.0 =<< openCamara qrBtnE
      resQRcodeE <- fmap getAddrFromUri <$> waiterResultCamera openE
      pasteE <- clipboardPaste pasteBtnE
      pure (recipientD, void setValE)
    else mdo
      (recipientD, events) <- labeledTextFieldWithBtns RecipientString initRecipient M.empty [mkIconBtn "fas fa-paste"] pasteE never errsD
      let pasteBtnE = head events
          setValE = pasteE
      pasteE <- clipboardPaste pasteBtnE
      pure (recipientD, void setValE)
  where mkIconBtn iconClass = elClass "i" iconClass blank
