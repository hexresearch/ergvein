module Ergvein.Wallet.Page.Initial(
    initialPage
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Wrapper

data GoPage = GoSeed | GoRestore

initialPage :: MonadFront t m => m ()
initialPage = wrapper $ divClass "initial-options" $ do
  newE <- fmap (GoSeed <$) $ row . outlineButton $ pure "Create wallet"
  restoreE <- fmap (GoRestore <$) $ row . outlineButton $ pure "Restore wallet"
  let goE = leftmost [newE, restoreE]
  void $ nextWidget $ ffor goE $ \go -> Retractable {
      retractableNext = case go of
        GoSeed -> mnemonicPage
        GoRestore -> initialPage -- TODO: here insert widget for restore page
    , retractablePrev = Just $ pure initialPage
    }
