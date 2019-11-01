module Ergvein.Wallet.Page.Initial(
    initialPage
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Language
import Reflex.Localize

import Control.Monad.IO.Class
import Data.Time

data GoPage = GoSeed | GoRestore

data InitialPageStrings =
    IPSCreate
  | IPSRestore

instance LocalizedPrint InitialPageStrings where
  localizedShow l v = case l of
    English -> case v of
      IPSCreate   -> "Create wallet"
      IPSRestore  -> "Restore wallet"
    Russian -> case v of
      IPSCreate   -> "Создать кошелёк"
      IPSRestore  -> "Восстановить кошелёк"

initialPage :: MonadFront t m => m ()
initialPage = wrapper True $ divClass "initial-options" $ do
  newE <- fmap (GoSeed <$) $ row . outlineButton $ IPSCreate
  restoreE <- fmap (GoRestore <$) $ row . outlineButton $ IPSRestore
  let goE = leftmost [newE, restoreE]
  now <- liftIO $ getCurrentTime
  postError $ (ErrorInfo ErrorTypeFail 10 ["Debug"] now $ ("Panic" :: Text)) <$ goE
  void $ nextWidget $ ffor never $ \go -> Retractable {
      retractableNext = case go of
        GoSeed -> mnemonicPage
        GoRestore -> initialPage -- TODO: here insert widget for restore page
    , retractablePrev = Just $ pure initialPage
    }
