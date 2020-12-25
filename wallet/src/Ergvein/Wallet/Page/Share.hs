{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Share(
    sharePage
  ) where

import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Clipboard (clipboardCopy)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Share
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.QRCode
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Share
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Network.Haskoin.Address.Base58

#ifdef ANDROID
import Ergvein.Wallet.Share
#endif

sharePage :: MonadFront t m => Currency -> m ()
sharePage cur = do
  title <- localized $ ShareTitle cur
  wrapper False title (Just $ pure $ sharePage cur) $ do
    pubStorage <- getPubStorage
    let xPubKeyMb  = pubKeystore'master . _currencyPubStorage'pubKeystore
          <$> M.lookup cur (_pubStorage'currencyPubStorages pubStorage)
        addressMb  = egvXPubKeyToEgvAddress <$> xPubKeyMb
    maybe errorPage renderPage addressMb
    pure ()
  where
    errorPage :: MonadFront t m => m ()
    errorPage = do
      pure ()

    generateURL :: Base58 -> Text
    generateURL addrB58 = curprefix cur <> addrB58

    renderPage :: MonadFront t m => EgvAddress -> m ()
    renderPage addr = do
      let addrBase  = egvAddrToString addr
      let shareAddr = addrBase
          shareUrl  = generateURL shareAddr
      vertSpacer
      divClass "share-qrcode-container" $ qrCodeWidget 252 252 (curprefix cur <> shareAddr)
      (e,_) <- elAttr' "div" [("class","share-block-value")] $ mapM_ (\v -> text v >> br) $ T.chunksOf 17 $ shareAddr
      let copyLineE = shareUrl <$ domEvent Click e
      vertSpacer
#ifdef ANDROID
      divClass "share-buttons-wrapper" $ do
        copyButE <- fmap (shareUrl <$) $ outlineTextIconButton ShareCopy "fas fa-copy"
        void $ clipboardCopy $ leftmost [copyLineE, copyButE]
        shareE <- fmap (shareUrl <$) $ outlineTextIconButton ShareShare "fas fa-share-alt"
        void $ shareShareUrl shareE
#else
      divClass "" $ do
        copyButE <- fmap (shareUrl <$) $ outlineTextIconButton ShareCopy "fas fa-copy"
        void $ clipboardCopy $ leftmost [copyLineE, copyButE]
#endif

vertSpacer :: MonadFrontBase t m => m ()
vertSpacer = divClass "share-v-spacer" blank
