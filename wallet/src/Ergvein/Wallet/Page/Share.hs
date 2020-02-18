{-# LANGUAGE OverloadedLists #-}
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
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Share (shareShareUrl)
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
      let addrBase   = addrToString (getCurrencyNetwork cur) addr
      let shareAdd   = addrBase
          shareMoney = Money cur 1
      let tempUrl = generateURL shareAdd shareMoney
      textLabel ShareLink $ text tempUrl
      copyE <- fmap (tempUrl <$) $ outlineButton ShareCopy
      --_ <- clipboardCopy copyE
      _ <- shareShareUrl copyE
      pure ()

    generateURL :: Base58 -> Money -> Text
    generateURL addrB58 money = case cur of
      BTC   -> "bitcoin:" <> addrB58 <> "?amount=" <> (showMoneyUnit money units)
      -- TODO: Fix URL for Ergo
      ERGO  -> ""

    units :: Units
    units = Units {
        unitBTC  = Just BtcWhole
      , unitERGO = Just ErgWhole
      }

textLabel :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> m a -- ^ Value
  -> m ()
textLabel lbl val = do
  i <- genId
  label i $ localizedText lbl
  elAttr "div" [("class","share-block-value"),("id",i)] $ val
  pure ()
