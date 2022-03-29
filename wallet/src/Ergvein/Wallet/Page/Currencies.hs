{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Currencies(
    selectCurrenciesPage
  , selectCurrenciesWidget
  ) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Reflex.Localize.Dom

import Ergvein.Crypto.Keys
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Restore
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements

-- As long as we only have one active currency, this widget is not needed
selectCurrenciesPage :: MonadFrontBase t m => WalletSource -> Bool -> Mnemonic -> m ()
selectCurrenciesPage wt seedBackupRequired mnemonic = wrapperSimple True $ do
  e <- selectCurrenciesWidget []
  void $ nextWidget $ ffor e $ \ac -> Retractable {
      retractableNext = if isAndroid
        -- On Android login is entered first. After that user is redirected to the password setup page.
        then setupLoginPage wt seedBackupRequired Nothing mnemonic ac Nothing
        -- On desktop login and passwrod are entered on the same page.
        else setupPasswordPage wt seedBackupRequired Nothing mnemonic ac Nothing Nothing
    , retractablePrev = Nothing
    }

selectCurrenciesWidget :: MonadFrontBase t m => [Currency] -> m (Event t [Currency])
selectCurrenciesWidget currs = mdo
  divClass "select-currencies-title" $ h4 $ localizedText CurTitle
  eL <- traverse (\(cur,_) -> divClass "currency-toggle" $ do
    let curD = fmap (toggled . snd .  fromMaybe (cur, False) . find (\(c,_) -> c == cur)) curListD
    e <- divButton curD (text $ showt cur)
    pure (cur <$ e)) startList
  curListD <- holdDyn startList $ poke (leftmost eL) $ \cur -> do
    curListS <- sampleDyn curListD
    pure $ fmap (invert cur) curListS
  let curButtonD = fmap (enabled . checkTrue) curListD
  btnE <- divButton curButtonD $ localizedText CurOk
  curListE <- performEvent $ ffor btnE $ \_ -> do
    curList <- sampleDyn curListD
    pure $ map fst $ filter snd curList
  pure $ gate (current (fmap (enabledGate . checkTrue) curListD)) curListE
  where
    checkTrue = find snd

    invert a (b,c) = if b == a
      then (b, not c)
      else (b, c)

    toggled b = if b
      then "button button-on button-currency"
      else "button button-off button-currency"

    enabled b = case b of
      Just _ -> "button button-outline"
      Nothing -> "button button-not-working"

    enabledGate b = case b of
      Just _ -> True
      Nothing -> False

    startList = ffor allCurrencies $ \cur -> if cur `elem` currs
      then (cur,True)
      else (cur,False)
