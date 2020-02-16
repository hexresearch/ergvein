{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Currencies(
    ActiveCurrencies(..)
  , loadActiveCurrencies
  , saveActiveCurrencies
  ) where

import Control.Monad.Random.Strict
import Data.Aeson
import Data.Text
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Directory

import qualified Data.Map.Strict as Map

import Ergvein.Aeson
import Ergvein.Crypto.Keys
import Ergvein.Text
import Ergvein.Types.Currency

#ifdef ANDROID
import Android.HaskellActivity
#endif

data ActiveCurrencies = ActiveCurrencies {
  activeCurrenciesMap  :: Map.Map Text [Currency]
} deriving (Eq, Show)

$(deriveJSON (aesonOptionsStripPrefix "") ''ActiveCurrencies)

#ifdef ANDROID

loadActiveCurrencies :: MonadIO m => m ActiveCurrencies
loadActiveCurrencies = liftIO $ do
  mpath <- liftIO $ getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      let acpath = path <> "/currencies.yaml"
      ex <- liftIO $ doesFileExist acpath
      if not ex
        then pure (ActiveCurrencies (Map.fromList []))
        else do
          mPT <- liftIO $ decodeFileStrict' acpath
          case mPT of
            Just p -> pure p
            Nothing -> pure (ActiveCurrencies (Map.fromList []))

saveActiveCurrencies :: MonadIO m => ActiveCurrencies -> m ()
saveActiveCurrencies ac = do
  mpath <- liftIO $ getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      let acpath = path <> "/currencies.yaml"
      ex <- liftIO $ doesFileExist acpath
      liftIO $ encodeFile acpath $ ac

#else
loadActiveCurrencies :: MonadIO m => m ActiveCurrencies
loadActiveCurrencies = liftIO $ do
  home <- getHomeDirectory
  let acpath = home <> "/.ergvein/currencies.yaml"
  ex <- doesFileExist acpath
  if not ex
    then pure (ActiveCurrencies (Map.fromList []))
    else do
      mPT <- liftIO $ decodeFileStrict' acpath
      case mPT of
        Just p -> pure p
        Nothing -> pure (ActiveCurrencies (Map.fromList []))

saveActiveCurrencies :: MonadIO m => ActiveCurrencies -> m ()
saveActiveCurrencies ac = liftIO $ do
  home <- getHomeDirectory
  let acpath = home <> "/.ergvein/currencies.yaml"
  ex <- liftIO $ doesFileExist acpath
  liftIO $ encodeFile acpath $ ac

#endif
