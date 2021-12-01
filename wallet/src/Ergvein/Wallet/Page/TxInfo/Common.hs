{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.TxInfo.Common(
    infoPageElement
  , infoPageElementEl
  , infoPageElementExpEl
  , symb
  , symbCol
  , transTypeCol
  , makeTxIdLink
  , makeNumberedTxIdLink
  ) where

import Data.Text (Text)

import Ergvein.Text
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Sepulcas.Elements

import qualified Data.Text as T

infoPageElement :: MonadFront t m => HistoryPageStrings -> Text -> m ()
infoPageElement hps txt = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  par $ text txt

infoPageElementEl :: MonadFront t m => HistoryPageStrings -> m () -> m ()
infoPageElementEl hps m = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  m

infoPageElementExpEl :: MonadFront t m => HistoryPageStrings -> m () -> m ()
infoPageElementExpEl hps m = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  parClass "tx-info-page-expanded" m

symb :: MonadFront t m => TransType -> m a -> m a
symb txType ma = case txType of
  TransRefill -> do
    spanClass "history-page-sign-icon" $ elClass "i" "fas fa-plus fa-fw" blank
    ma
  TransWithdraw -> do
    spanClass "history-page-sign-icon" $ elClass "i" "fas fa-minus fa-fw" blank
    ma

symbCol :: MonadFront t m => TransType -> m a -> m a
symbCol txType ma = divClass ("history-amount-" <> (T.toLower . showt) txType) $ do
  case txType of
    TransRefill -> do
      spanClass "history-page-sign-icon" $ elClass "i" "fas fa-plus fa-fw" blank
      ma
    TransWithdraw -> do
      spanClass "history-page-sign-icon" $ elClass "i" "fas fa-minus fa-fw" blank
      ma

transTypeCol :: MonadFront t m => TransType -> m a -> m a
transTypeCol txType = divClass ("history-amount-" <> (T.toLower . showt) txType)

makeTxIdLink :: MonadFront t m => Text -> m ()
makeTxIdLink txIdText = do
  settings <- getSettings
  let urlPrefixes = btcSettings'explorerUrls $ getBtcSettings settings
      urlPrefix = if isTestnet then testnetUrl urlPrefixes else mainnetUrl urlPrefixes
  hyperlink "link" txIdText (urlPrefix <> "/tx/" <> txIdText)

makeNumberedTxIdLink :: MonadFront t m => Currency -> (Int, TxId) -> m ()
makeNumberedTxIdLink currency (num, txId) = do
  settings <- getSettings
  let txIdText = egvTxHashToStr txId
      urlPrefixes = case currency of
          BTC -> btcSettings'explorerUrls $ getBtcSettings settings
      urlPrefix = if isTestnet then testnetUrl urlPrefixes else mainnetUrl urlPrefixes
  text $ showt num <> ". "
  hyperlink "link" txIdText (urlPrefix <> "/tx/" <> txIdText)
  br
