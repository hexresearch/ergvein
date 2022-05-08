{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.TxInfo.Remove(
    removeTxPage
  ) where

import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Word

import Ergvein.Core.Transaction.View.Btc
import Ergvein.Maybe
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Widget.Input.Fee
import Ergvein.Wallet.Wrapper
import Sepulcas.Alert
import Sepulcas.Elements

import Network.Haskoin.Network (Inv(..), InvVector(..), InvType(..), Message(..))

import qualified Data.List as L
import qualified Network.Haskoin.Transaction as HT

removeTxPage :: MonadFront t m => Currency -> TxId -> m ()
removeTxPage cur tx = do
  title <- localized RemoveTxTitle
  let thisWidget = Just . pure $ removeTxPage cur tx
  wrapper False title thisWidget $ divClass "remove-tx-page" $ do
    elClass "h4" "mb-1" $ localizedText RemoveTxHeader
    elClass "h5" "mb-1" $ localizedText RemoveTxBody
    divClass "fit-content ml-a mr-a" $ do
        cancelE <- outlineButton RemoveTxCancel
        removeE <- outlineButton RemoveTxAgree
        removedE' <- removeOutgoingTxs "removeTxPage" cur $ [tx] <$ removeE
        removedE <- removeStorageTx "removeTxPage" cur $ tx <$ removedE'
        void $ retract $ leftmost [cancelE, removedE]
