{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Control.Lens
import Data.Maybe (fromMaybe, isJust)
import Data.Word
import Network.Haskoin.Address
import Network.Haskoin.Transaction
import Network.Wreq

import Ergvein.Filters.Btc
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Worker.Node
import Ergvein.Wallet.Wrapper

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

#ifdef ANDROID
import Control.Monad.IO.Class
#endif

data BalancesStrings
  = BalancesTitle
  | ButtonSend
  | ButtonReceive

instance LocalizedPrint BalancesStrings where
  localizedShow l v = case l of
    English -> case v of
      BalancesTitle -> "Default wallet"
      ButtonSend    -> "Send"
      ButtonReceive -> "Receive"
    Russian -> case v of
      BalancesTitle -> "Стандартный кошелек"
      ButtonSend    -> "Отправить"
      ButtonReceive -> "Получить"

balancesPage :: MonadFront t m => m ()
balancesPage = do
  walletName <- getWalletName
  title <- localized walletName
  wrapper False title (Just $ pure balancesPage) $ do
    syncWidget =<< getSyncProgress
    currenciesList walletName

currenciesList :: MonadFront t m => Text -> m ()
currenciesList name = divClass "currency-content" $ do
  s <- getSettings
  ps <- getPubStorage
  pubSD <- getPubStorageD
  let currencies = _pubStorage'activeCurrencies ps
      thisWidget = Just $ pure balancesPage
  if L.length currencies == 1
    then do
      buildE <- getPostBuild
      void $ nextWidget $ ffor buildE $ \_ -> Retractable {
        retractableNext = historyPage $ L.head currencies
      , retractablePrev = Nothing
      }
    else do
      historyE <- leftmost <$> traverse (currencyLine s) currencies
      if (settingsPortfolio s)
        then portfolioWidget
        else pure ()
      void $ nextWidget $ ffor historyE $ \cur -> Retractable {
        retractableNext = historyPage cur
      , retractablePrev = thisWidget
      }
  where
    currencyLine settings cur = do
      (e, _) <- divClass' "currency-row" $ do
        bal <- balancesWidget cur
        let setUs = getSettingsUnits settings
        divClass "currency-name"    $ text $ currencyName cur
        divClass "currency-balance" $ do
          elClass "span" "currency-value" $ dynText $ (\v -> showMoneyUnit v setUs) <$> bal
          elClass "span" "currency-unit"  $ text $ symbolUnit cur setUs
          elClass "span" "currency-arrow" $ text "〉"
      pure $ cur <$ domEvent Click e
    getSettingsUnits = fromMaybe defUnits . settingsUnits
