{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.History.Common(
  noTxsPlaceholder
) where

import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Sepulcas.Elements

noTxsPlaceholder :: MonadFront t m => m ()
noTxsPlaceholder = divClass "history-empty-placeholder text-muted" $ do
  par $ localizedText HistoryNoTxs