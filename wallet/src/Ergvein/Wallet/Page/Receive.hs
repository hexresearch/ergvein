{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Receive (
    receivePage
  ) where

import Control.Lens

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Receive
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Page.QRCode
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Wrapper

receivePage :: MonadFront t m => Currency -> m ()
receivePage cur = do
  pubStoreD <- getPubStorageD
  let keyD = ffor pubStoreD $ \ps ->
        (getLastUnusedKey . _currencyPubStorage'pubKeystore) =<< (ps ^. pubStorage'currencyPubStorages . at cur)
  widgetHoldDyn $ ffor keyD $ \case
    Nothing -> exceededGapLimit cur
    Just (i, key) -> receivePageWidget cur i key
  pure ()

mockAddress :: Text
mockAddress = "1BoatSLRHtKNngkdXEeobR76b53LETtpyT"

exceededGapLimit :: MonadFront t m => Currency -> m ()
exceededGapLimit cur = wrapper (ReceiveTitle cur) (Just $ pure $ receivePage cur) False $ do
  h2 $ localizedText RPSGap

#ifdef ANDROID
receivePageWidget :: MonadFront t m => Currency -> Int -> EgvExternalKeyBox -> m ()
receivePageWidget cur i EgvExternalKeyBox{..} = wrapper (ReceiveTitle cur) (Just $ pure $ receivePage cur) False $ do
  let thisWidget = Just $ pure $ receivePage cur
  navbarWidget cur thisWidget NavbarReceive
  void $ divClass "centered-wrapper" $ divClass "centered-content" $ do
    divClass "receive-qr-andr"   $ qrCodeWidget keyTxt cur
    newE  <- buttonClass "button button-outline button-receive" RPSGenNew
    copyE <- buttonClass "button button-outline button-receive" RPSCopy
    setFlatToExtPubKey $ (cur, i) <$ newE
    showInfoMsg =<< clipboardCopy (keyTxt <$ copyE)
    divClass "receive-adr-andr" $ text $ "#" <> showt i <> ": " <> keyTxt
    labelD <- divClass "button-receive" $ textFieldNoLabel $ getLabelFromEgvPubKey extKeyBox'key
    btnE <- buttonClass "button button-outline button-receive" RPSAddLabel
    setLabelToExtPubKey $ attachWith (\l _ -> (cur, i, l)) (current labelD) btnE
  where
    keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress extKeyBox'key

#else
receivePageWidget :: MonadFront t m => Currency -> Int -> EgvExternalKeyBox -> m ()
receivePageWidget cur i EgvExternalKeyBox{..} = wrapper (ReceiveTitle cur) (Just $ pure $ receivePage cur) False $ do
  let thisWidget = Just $ pure $ receivePage cur
  navbarWidget cur thisWidget NavbarReceive
  void $ divClass "centered-wrapper" $ divClass "centered-content" $ do
    divClass "receive-qr"   $ qrCodeWidget keyTxt cur
    divClass "receive-buttons-wrapper" $ do
       newE  <- outlineButton RPSGenNew
       copyE <- outlineButton RPSCopy
       setFlatToExtPubKey $ (cur, i) <$ newE
       showInfoMsg =<< clipboardCopy (keyTxt <$ copyE)
    divClass "receive-adr" $ text $ "#" <> showt i <> ": " <> keyTxt
    divClass "label-block" $ do
      labelD <- textFieldNoLabel $ getLabelFromEgvPubKey extKeyBox'key
      btnE <- buttonClass (pure "button button-outline label-block-button") RPSAddLabel
      setLabelToExtPubKey $ attachWith (\l _ -> (cur, i, l)) (current labelD) btnE
      pure ()
  where
    keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress extKeyBox'key
#endif
