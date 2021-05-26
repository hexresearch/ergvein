{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Ergo.Send (
    sendPageErg
  ) where

import Data.Word

import Ergvein.Types
import Sepulcas.Elements
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types

sendPageErg :: MonadFront t m => Maybe ((UnitERGO, Word64), (FeeMode, Word64), ErgAddress) -> m ()
sendPageErg mInit = mdo
  walletName <- getWalletName
  title <- localized walletName
  let navbar = if isAndroid
        then blank
        else navbarWidget ERGO thisWidget NavbarSend
      thisWidget = Just $ sendPageErg <$> retInfoD
  retInfoD <- sendWidget mInit title navbar thisWidget
  pure ()

sendWidget :: MonadFront t m
  => Maybe ((UnitERGO, Word64), (FeeMode, Word64), ErgAddress)
  -> Dynamic t Text
  -> m a
  -> Maybe (Dynamic t (m ()))
  -> m (Dynamic t (Maybe ((UnitERGO, Word64), (FeeMode, Word64), ErgAddress)))
sendWidget = undefined