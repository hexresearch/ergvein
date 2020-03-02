{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Share(
    sharePage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Clipboard (clipboardCopy)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Id
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Share
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.QRCode
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Share
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.Haskoin.Address
import Network.Haskoin.Address.Base58
import Network.Haskoin.Keys

sharePage :: MonadFront t m => Currency -> m ()
sharePage cur = do
  let thisWidget = Just $ pure $ sharePage cur
  menuWidget (ShareTitle cur) thisWidget
  wrapper False $ divClass "share-content" $ do
    pks :: PublicKeystore <- getPublicKeystore
    let xPubKeyMb  = (egvXPubKey . egvPubKeyсhain'master) <$> M.lookup cur pks
        addressMb  = xPubAddr <$> xPubKeyMb
    maybe errorPage renderPage addressMb
    pure ()
  where
    errorPage :: MonadFront t m => m ()
    errorPage = do
      pure ()

    renderPage :: MonadFront t m => Address -> m ()
    renderPage addr = do
      let addrBase  = addrToString (getCurrencyNetwork cur) addr
      let shareAddr = addrBase
          shareUrl  = generateURL shareAddr
      vertSpacer
      qrCodeWidget shareAddr cur
      gpbE <- delay 0.1 =<< getPostBuild
      void $ widgetHold blank $ ffor gpbE $ \_ -> do
        textBase64 <- genQrCodeBase64Image
        --elAttr "img" [("src",textBase64), ("class","share-image-qrcode")] blank
        elAttr "div" [("class","share-block-value")] $ mapM_ (\v -> text v >> br) $ T.chunksOf 24 $ shareUrl
        vertSpacer
        divClass "initial-options grid1" $ do
          copyE <- fmap (shareAddr <$) $ outlineButton ShareCopy
          _ <- clipboardCopy copyE
#ifdef ANDROID
          shareE <- fmap (shareUrl <$) $ outlineButton ShareShare
          _ <- shareShareUrl shareE
#endif
          pure ()
      pure ()

    generateURL :: Base58 -> Text
    generateURL addrB58 = case cur of
      BTC   -> "bitcoin://" <> addrB58
      ERGO  -> "ergo://" <> addrB58

    units :: Units
    units = Units {
        unitBTC  = Just BtcWhole
      , unitERGO = Just ErgWhole
      }

vertSpacer :: MonadFrontBase t m => m ()
vertSpacer = divClass "share-v-spacer" blank
