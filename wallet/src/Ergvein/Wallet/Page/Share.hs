{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Share(
    sharePage
  ) where

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
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
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Wrapper

import Data.Maybe (fromMaybe)
import Network.Haskoin.Address
import Network.Haskoin.Address.Base58
import Network.Haskoin.Keys
import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

sharePage :: MonadFront t m => Currency -> m ()
sharePage cur = wrapper (ShareTitle cur) (Just $ pure $ sharePage cur) False $ divClass "share-content" $ do
  pubStorage <- getPubStorage
  let xPubKeyMb = case cur of
        BTC -> getXPubKeyMb BTCTag pubStorage
        ERGO -> getXPubKeyMb ERGTag pubStorage
      addressMb = egvXPubKeyToEgvAddress <$> xPubKeyMb
  maybe errorPage renderPage addressMb
  pure ()
  where
    getXPubKeyMb :: CurrencyTag tx -> PubStorage -> Maybe EgvXPubKey
    getXPubKeyMb curTag pubStorage = pubKeystore'master . _currencyPubStorage'pubKeystore
      <$> DM.lookup curTag (_pubStorage'currencyPubStorages pubStorage)

    errorPage :: MonadFront t m => m ()
    errorPage = do
      pure ()

    renderPage :: MonadFront t m => EgvAddress -> m ()
    renderPage addr = do
      let addrBase  = egvAddrToString addr
      let shareAddr = addrBase
          shareUrl  = generateURL shareAddr
      vertSpacer
      divClass "share-qrcode-container" $ qrCodeWidget shareAddr cur
      (e,_) <- elAttr' "div" [("class","share-block-value")] $ mapM_ (\v -> text v >> br) $ T.chunksOf 17 $ shareAddr
      let copyLineE = shareUrl <$ domEvent Click e
      vertSpacer
#ifdef ANDROID
      divClass "share-buttons-wrapper" $ do
#else
      divClass "" $ do
#endif
        copyButE <- fmap (shareUrl <$) $ outlineButtonWithIcon ShareCopy "fas fa-copy"
        _ <- clipboardCopy $ leftmost [copyLineE, copyButE]
#ifdef ANDROID
        shareE <- fmap (shareUrl <$) $ outlineButtonWithIcon ShareShare "fas fa-share-alt"
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
