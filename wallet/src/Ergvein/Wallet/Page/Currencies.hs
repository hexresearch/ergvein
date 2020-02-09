{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Currencies(
    selectCurrenciesPage
  , selectCurrenciesWidget
  ) where

import Control.Monad.Random.Strict
import Data.Bifunctor
import Data.List (find)
import Data.Maybe (fromMaybe)
import Ergvein.Crypto.Keys
import Ergvein.Crypto.WordLists
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
  nextWidget $ ffor (m <$ e) $ \m -> Retractable {
#ifdef ANDROID
      retractableNext = setupLoginPage m
#else
      retractableNext = passwordPage m
#endif
    , retractablePrev = Just $ pure $ selectCurrenciesPage m
    }
  pure ()

selectCurrenciesWidget :: MonadFrontBase t m => m (Event t ())
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
  let curButtonD = fmap (enabled . (find (\(_,b) -> b == True))) curListD
      curGateD = fmap (enabled . (find (\(_,b) -> b == True))) curListD
  btnE <- divButton curButtonD $ localizedText CurOk
  pure $ gate (current (fmap (enabledGate . (find (\(_,b) -> b == True))) curListD)) btnE
  where
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
