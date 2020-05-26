module Ergvein.Wallet.Page.Receive (
    receivePage
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Receive
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Page.QRCode
import Ergvein.Wallet.Wrapper

receivePage :: MonadFront t m => Currency -> m ()
receivePage cur = wrapper (ReceiveTitle cur) (Just $ pure $ receivePage cur) False $ do
  let thisWidget = Just $ pure $ receivePage cur
  navbarWidget cur thisWidget NavbarReceive
  void $ divClass "centered-wrapper" $ divClass "centered-content" $ do
    divClass "receive-qr"   $ qrCodeWidget mockAddress cur
    (newE,copyE) <- divClass "receive-buttons-wrapper" $ do
       newE <- outlineButton RPSGenNew
       copyE <- outlineButton RPSCopy
       pure (newE, copyE)
    divClass "receive-adr" $ text mockAddress
    divClass "label-block" $ do
      let emptyStr :: Text = ""
      labelD <- divClass "label-block-input" $ textField emptyStr ""
      btnE <- buttonClass (pure "button button-outline label-block-button") RPSAddLabel
      pure (labelD,btnE)

mockAddress :: Text
mockAddress = "1BoatSLRHtKNngkdXEeobR76b53LETtpyT"
