module Ergvein.Wallet.Page.Restore(
    restorePage
  ) where

import Ergvein.Text
import Ergvein.Filters.Btc
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Worker.Node
import Ergvein.Wallet.Wrapper

restorePage :: MonadFront t m =>  m ()
restorePage = wrapperSimple True $ void $ workflow heightAsking
  where
    heightAsking = Workflow $ do
      el "h3" $ text "Getting current height"
      heightD <- getCurrentHeight BTC
      height0E <- tag (current heightD) <$> getPostBuild
      let heightE = leftmost [updated heightD, height0E]
      let nextE = fforMaybe heightE $ \h -> if h == 0 then Nothing else Just downloadFilters
      pure ((), nextE)

    downloadFilters = Workflow $ do
      el "h3" $ text "Downloading filters"
      filtersD <- watchFiltersHeight BTC
      heightD <- getCurrentHeight BTC
      el "h4" $ dynText $ do
        filters <- filtersD
        height <- heightD
        let pct = fromIntegral filters / fromIntegral height :: Float
        -- pure $ showt filters <> "/" <> showt height <> " " <> showf 2 (100 * pct) <> "%"
        pure $ showf 2 (100 * pct) <> "%"
      pure ((), never)
