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
receivePage cur = wrapper (RecieveTitle cur) (Just $ pure $ receivePage cur) False $ do
  let thisWidget = Just $ pure $ receivePage cur
  navbarWidget cur thisWidget NavbarReceive
  void $ divClass "centered-wrapper" $ divClass "centered-content" $ do
    divClass "recieve-qr"   $ qrCodeWidget mockAddress cur
    divClass "recieve-adr" $ text mockAddress
    divClass "label-block" $ do
      let emptyStr :: Text = ""
      labelD <- divClass "label-block-input" $ textField emptyStr ""
      btnE <- divClass "label-block-button" $ outlineButton RPSAddLabel
      pure (labelD,btnE)

mockAddress :: Text
mockAddress = "1BoatSLRHtKNngkdXEeobR76b53LETtpyT"
