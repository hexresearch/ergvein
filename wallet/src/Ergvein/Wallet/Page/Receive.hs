{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Receive (
    receivePage
  ) where

import Control.Lens

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Elements.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Receive
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Page.Canvas
import Ergvein.Wallet.Page.QRCode
import Ergvein.Wallet.Share
import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Wrapper

import qualified Data.Text as T
import qualified Data.List as L

#ifdef ANDROID
import Ergvein.Wallet.Share
#endif

receivePage :: MonadFront t m => Currency -> m ()
receivePage cur = do
  pubStoreD <- getPubStorageD
  let lastUnusedKeyD = ffor pubStoreD $ \ps ->
        (getLastUnusedKey External . _currencyPubStorage'pubKeystore) =<< (ps ^. pubStorage'currencyPubStorages . at cur)
  widgetHoldDyn $ ffor lastUnusedKeyD $ \case
    Nothing -> exceededGapLimit cur
    Just (i, key) -> receivePageWidget cur i key
  pure ()

exceededGapLimit :: MonadFront t m => Currency -> m ()
exceededGapLimit cur = do
  walletName <- getWalletName
  title <- localized walletName
  wrapper True title (Just $ pure $ receivePage cur) $ do
    h2 $ localizedText RPSGap

#ifdef ANDROID
receivePageWidget :: MonadFront t m => Currency -> Int -> EgvPubKeyBox -> m ()
receivePageWidget cur i EgvPubKeyBox{..} = do
  walletName <- getWalletName
  title <- localized walletName
  let thisWidget = Just $ pure $ receivePage cur
      navbar = blank
  wrapperNavbar False title thisWidget navbar $ void $ divClass "receive-page" $ do
    base64D <- divClass "receive-qr" $ qrCodeWidgetWithData qrSizeMedium prefixedKeyText
    (newE, copyE, shareE) <- divClass "receive-buttons-wrapper" $ do
      nE  <- newAddrBtn
      cE <- copyAddrBtn
      sE <- fmap (prefixedKeyText <$) shareAddrBtn
      shareQRE <- shareQRBtn
      shareShareQR $ attachWithMaybe (\m _ -> (, keyTxt) <$> m) (current base64D) shareQRE
      pure (nE, cE, sE)
    _ <- shareShareUrl shareE
    setFlagToExtPubKey "receivePageWidget:1" $ (cur, i) <$ newE
    clipboardCopy (keyTxt <$ copyE)
    divClass "receive-adr-andr" $ text $ "#" <> showt i <> ": " <> keyView
    labelD <- divClass "button-receive" $ textFieldNoLabel $ getLabelFromEgvPubKey pubKeyBox'key
    btnE <- labelAddrBtn
    setLabelToExtPubKey "receivePageWidget:2" $ attachWith (\l _ -> (cur, i, l)) (current labelD) btnE
  where
    keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress pubKeyBox'key
    keyView = T.pack $ L.intercalate " " $ mkChunks 4 $ T.unpack keyTxt
    prefixedKeyText = curprefix cur <> keyTxt

shareAddrBtn :: MonadFront t m => m (Event t ())
shareAddrBtn = divClass "receive-btn-wrapper" $ outlineTextIconButton CSShare "fas fa-share-alt fa-lg"

shareQRBtn :: MonadFront t m => m (Event t ())
shareQRBtn = divClass "receive-btn-wrapper" $ outlineTextIconButtonTypeButton CSShareQR "fas fa-qrcode fa-lg"

#else
receivePageWidget :: MonadFront t m => Currency -> Int -> EgvPubKeyBox -> m ()
receivePageWidget cur i EgvPubKeyBox{..} = do
  walletName <- getWalletName
  title <- localized walletName
  let thisWidget = Just $ pure $ receivePage cur
      navbar = navbarWidget cur thisWidget NavbarReceive
  wrapperNavbar False title thisWidget navbar $ void $ divClass "receive-page" $ do
    void $ divClass "receive-qr" $ qrCodeWidget qrSizeMedium (curprefix cur <> keyTxt)
    void $ divClass "receive-buttons-wrapper" $ do
      newE  <- newAddrBtn
      copyE <- copyAddrBtn
      setFlagToExtPubKey "receivePageWidget:1" $ (cur, i) <$ newE
      clipboardCopy (keyTxt <$ copyE)
    divClass "receive-adr" $ text $ "#" <> showt i <> ": " <> keyView
    divClass "label-block" $ do
      labelD <- textFieldNoLabel $ getLabelFromEgvPubKey pubKeyBox'key
      btnE <- labelAddrBtn
      setLabelToExtPubKey "receivePageWidget:2" $ attachWith (\l _ -> (cur, i, l)) (current labelD) btnE
      pure ()
  where
    keyTxt = egvAddrToString $ egvXPubKeyToEgvAddress pubKeyBox'key
    keyView = T.pack $ L.intercalate " " $ mkChunks 4 $ T.unpack keyTxt
#endif

newAddrBtn :: MonadFront t m => m (Event t ())
newAddrBtn = divClass "receive-btn-wrapper" $ outlineTextIconButton RPSGenNew "fas fa-forward fa-lg"

copyAddrBtn :: MonadFront t m => m (Event t ())
copyAddrBtn = divClass "receive-btn-wrapper" $ outlineTextIconButton CSCopy "fas fa-copy fa-lg"

labelAddrBtn :: MonadFront t m => m (Event t ())
labelAddrBtn = outlineTextIconButton RPSAddLabel "fas fa-tag fa-lg"
