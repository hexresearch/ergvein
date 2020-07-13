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
import Ergvein.Wallet.Share
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Wrapper

receivePage :: MonadFront t m => Currency -> m ()
receivePage cur = do
  pubStoreD <- getPubStorageD
  let keyD = ffor pubStoreD $ \ps ->
        (getLastUnusedKey External . _currencyPubStorage'pubKeystore) =<< (ps ^. pubStorage'currencyPubStorages . at cur)
  widgetHoldDyn $ ffor keyD $ \case
    Nothing -> exceededGapLimit cur
    Just (i, key) -> receivePageWidget cur i key
  pure ()

mockAddress :: Text
mockAddress = "1BoatSLRHtKNngkdXEeobR76b53LETtpyT"

exceededGapLimit :: MonadFront t m => Currency -> m ()
exceededGapLimit cur = do
  title <- balanceTitleWidget cur
  wrapper True title (Just $ pure $ receivePage cur) $ do
    h2 $ localizedText RPSGap

#ifdef ANDROID
receivePageWidget :: MonadFront t m => Currency -> Int -> EgvPubKeyBox -> m ()
receivePageWidget cur i EgvPubKeyBox{..} = do
  title <- balanceTitleWidget cur
  let thisWidget = Just $ pure $ receivePage cur
      navbar = navbarWidget cur thisWidget NavbarReceive
  wrapperNavbar False title thisWidget navbar $ void $ divClass "receive-page" $ do
    divClass "receive-qr-andr" $ qrCodeWidget keyTxt cur
    (newE, copyE, shareE) <- divClass "receive-buttons-wrapper" $ do
      nE  <- newAddrBtn
      cE <- copyAddrBtn
      sE <- fmap (shareUrl <$) shareAddrBtn
      pure (nE, cE, sE)
    _ <- shareShareUrl shareE
    setFlagToExtPubKey $ (cur, i) <$ newE
    showInfoMsg =<< clipboardCopy (keyTxt <$ copyE)
    divClass "receive-adr-andr" $ text $ "#" <> showt i <> ": " <> keyTxt
    labelD <- divClass "button-receive" $ textFieldNoLabel $ getLabelFromEgvPubKey pubKeyBox'key
    btnE <- labelAddrBtn
    setLabelToExtPubKey $ attachWith (\l _ -> (cur, i, l)) (current labelD) btnE
  where
    keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress pubKeyBox'key
    shareUrl = generateURL keyTxt
    generateURL :: Text -> Text
    generateURL addr = case cur of
      BTC  -> "bitcoin://" <> addr
      ERGO -> "ergo://" <> addr

#else
receivePageWidget :: MonadFront t m => Currency -> Int -> EgvPubKeyBox -> m ()
receivePageWidget cur i EgvPubKeyBox{..} = do
  title <- balanceTitleWidget cur
  let thisWidget = Just $ pure $ receivePage cur
      navbar = navbarWidget cur thisWidget NavbarReceive
  wrapperNavbar False title thisWidget navbar $ void $ divClass "receive-page" $ do
    divClass "receive-qr" $ qrCodeWidget keyTxt cur
    divClass "receive-buttons-wrapper" $ do
      newE  <- newAddrBtn
      copyE <- copyAddrBtn
      setFlagToExtPubKey $ (cur, i) <$ newE
      showInfoMsg =<< clipboardCopy (keyTxt <$ copyE)
    divClass "receive-adr" $ text $ "#" <> showt i <> ": " <> keyTxt
    divClass "label-block" $ do
      labelD <- textFieldNoLabel $ getLabelFromEgvPubKey pubKeyBox'key
      btnE <- labelAddrBtn
      setLabelToExtPubKey $ attachWith (\l _ -> (cur, i, l)) (current labelD) btnE
      pure ()
  where
    keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress pubKeyBox'key
#endif

newAddrBtn :: MonadFront t m => m (Event t ())
newAddrBtn = divClass "receive-btn-wrapper" $ outlineTextIconButton RPSGenNew "fas fa-forward fa-lg"

copyAddrBtn :: MonadFront t m => m (Event t ())
copyAddrBtn = divClass "receive-btn-wrapper" $ outlineTextIconButton RPSCopy "fas fa-copy fa-lg"

shareAddrBtn :: MonadFront t m => m (Event t ())
shareAddrBtn = divClass "receive-btn-wrapper" $ outlineTextIconButton RPSShare "fas fa-share-alt fa-lg"

labelAddrBtn :: MonadFront t m => m (Event t ())
labelAddrBtn = outlineTextIconButton RPSAddLabel "fas fa-tag fa-lg"
