{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Currencies(
    selectCurrenciesPage
  , selectCurrenciesWidget
  ) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Reflex.Localize

import Ergvein.Crypto.Keys
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Restore
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Localization.Currencies
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Wrapper

selectCurrenciesPage :: MonadFrontBase t m => WalletSource -> Mnemonic -> m ()
selectCurrenciesPage wt m = wrapperSimple True $ do
  -- TODO: remove this when ERGO is ready
  e <- fmap ([BTC] <$) getPostBuild
  -- uncomment this when ERGO is ready
  -- e <- selectCurrenciesWidget []
  void $ nextWidget $ ffor e $ \ac -> Retractable {
#ifdef ANDROID
      retractableNext = setupLoginPage wt m ac
#else
      retractableNext = passwordPage wt m ac
#endif
    , retractablePrev = Just $ pure $ selectCurrenciesPage wt m
    }

selectCurrenciesWidget :: MonadFrontBase t m => [Currency] -> m (Event t [Currency])
selectCurrenciesWidget currs = mdo
  divClass "select-currencies-title" $ h4 $ localizedText CurTitle
  eL <- traverse (\(cur,_) -> divClass "currency-toggle" $ do
    let curD = fmap (toggled . snd .  (fromMaybe (cur, False)) . (find (\(c,_) -> c == cur))) curListD
    e <- divButton curD (text $ showt cur)
    pure (cur <$ e) ) $ startList
  curListD <- holdDyn startList $ poke (leftmost eL) $ \cur -> do
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

    startList = ffor allCurrencies $ \cur -> if (elem cur currs)
      then (cur,True)
      else (cur,False)
