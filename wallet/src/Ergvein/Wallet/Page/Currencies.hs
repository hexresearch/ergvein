{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Currencies(
    selectCurrenciesPage
  , selectCurrenciesWidget
  ) where

import Control.Monad.Random.Strict
import Data.Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Directory

import qualified Data.Map.Strict as Map

import Ergvein.Aeson
import Ergvein.Crypto.Keys
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Input
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Localization.Currencies
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Validate
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Util
import Reflex.Localize

selectCurrenciesPage :: MonadFrontBase t m => Mnemonic -> m ()
selectCurrenciesPage m = wrapper True $ do
  e <- selectCurrenciesWidget
  nextWidget $ ffor e $ \ac -> Retractable {
#ifdef ANDROID
      retractableNext = setupLoginPage m ac
#else
      retractableNext = passwordPage m ac
#endif
    , retractablePrev = Just $ pure $ selectCurrenciesPage m
    }
  pure ()

selectCurrenciesWidget :: MonadFrontBase t m => m (Event t [Currency])
selectCurrenciesWidget = mdo
  divClass "select-currencies-title" $ h4 $ localizedText CurTitle
  let emptyList = zip allCurrencies (repeat False)
  eL <- traverse (\(cur,flag) -> divClass "currency-toggle" $ do
    let curD = fmap (toggled . snd .  (fromMaybe (cur, False)) . (find (\(c,_) -> c == cur))) curListD
    e <- divButton curD (text $ showt cur)
    pure (cur <$ e) ) emptyList
  curListD <- holdDyn emptyList $ poke (leftmost eL) $ \cur -> do
    curListS <- sampleDyn curListD
    pure $ fmap (invert cur) curListS
  let curButtonD = fmap (enabled . checkTrue) curListD
  btnE <- divButton curButtonD $ localizedText CurOk
  curListE <- performEvent $ ffor btnE $ \_ -> do
    curList <- sampleDyn curListD
    pure $ fst . unzip $ filter (\(_,b) -> b == True) curList
  pure $ gate (current (fmap (enabledGate . checkTrue) curListD)) curListE
  where
    checkTrue = (find (\(_,b) -> b == True))

    invert a (b,c) = if b == a
      then (b, not c)
      else (b, c)

    toggled b = if b
      then "button button-on"
      else "button button-off"

    enabled b = case b of
      Just _ -> "button button-outline"
      Nothing -> "button button-not-working"

    enabledGate b = case b of
      Just _ -> True
      Nothing -> False

{-
saveActiveCurrencies :: MonadIO m => [] -> m ()
saveActiveCurrencies pt = do
  mpath <- liftIO $ getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      let triespath = path <> "/tries.yaml"
      ex <- liftIO $ doesFileExist triespath
      liftIO $ encodeFile triespath $ pt

loadActiveCurrencies ::MonadIO m => m PatternTries
loadActiveCurrencies = do
  mpath <- liftIO $ getFilesDir =<< getHaskellActivity
  case mpath of
    Nothing -> fail "Ergvein panic! No local folder!"
    Just path -> do
      let triespath = path <> "/tries.yaml"
      ex <- liftIO $ doesFileExist triespath
      if not ex
        then pure (PatternTries (Map.fromList []))
        else do
          mPT <- liftIO $ decodeFileStrict' triespath
          case mPT of
            Just p -> pure p
            Nothing -> pure (PatternTries (Map.fromList []))
-}
