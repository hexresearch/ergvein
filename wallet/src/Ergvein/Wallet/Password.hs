{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Password(
    setupLoginPassword
  , askPassword
  , askPasswordModal
#ifdef ANDROID
  , setupLoginPattern
  , askPattern
  , askPatternModal
#endif
  ) where

import Control.Monad.Except
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Validate

import Reflex.Dom

import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)

setupLoginPassword :: MonadFrontBase t m => m (Event t (Text, Password))
setupLoginPassword = divClass "setup-password" $ form $ fieldset $ mdo
  loginD <- textField PWSLogin ""
  p1D <- passFieldWithEye PWSPassword
  p2D <- passFieldWithEye PWSRepeat
  e <- submitClass "button button-outline" PWSSet
  validate $ poke e $ const $ runExceptT $ do
    p1 <- sampleDyn p1D
    p2 <- sampleDyn p2D
    l  <- sampleDyn loginD
    check PWSEmptyLogin $ not $ T.null l
    check PWSNoMatch $ p1 == p2
    check PWSEmptyPassword $ not $ T.null p1
    pure (l,p1)

askPassword :: MonadFrontBase t m => m (Event t Password)
askPassword = divClass "ask-password" $ form $ fieldset $ do
  pD <- passFieldWithEye PWSPassword
  e <- submitClass "button button-outline" PWSGo
  pure $ tag (current pD) e

askPasswordModal :: MonadFrontBase t m => m ()
askPasswordModal = mdo
  goE   <- fmap fst getPasswordModalEF
  fire  <- fmap snd getPasswordSetEF
  let redrawE = leftmost [Just <$> goE, Nothing <$ passE]
  passE <- fmap (switch . current) $ widgetHold (pure never) $ ffor redrawE $ \case
    Just i -> divClass "ask-password-modal" $ (fmap . fmap) ((i,) . Just) askPassword
    Nothing -> pure never
  performEvent_ $ (liftIO . fire) <$> passE

#ifdef ANDROID
askPattern :: MonadFrontBase t m => m (Event t Password)
askPattern = divClass "ask-pattern" $ form $ fieldset $ mdo
  c <- loadCounter
  e2 <- tickLossyFromPostBuildTime 10
  now <- liftIO $ getCurrentTime
  a <- (clockLossy 1 now)
  s <- widgetHold (pure False) $ ffor (updated a) $ \TickInfo{..} -> if (10 - _tickInfo_n) > 0
    then do
      divClass "backcounter" $ text $ "You should wait " <>  (showt $ 10 - _tickInfo_n) <> " sec"
      pure False
    else do
      pure (True)
  pD <- patternAskWidget
  performEvent $ ffor (updated pD) $ \
  counterD <- holdDyn c $ poke (updated pD) $ \_ -> do
    cS <- sampleDyn counterD
    pure (cS + 1)
  divClass "counter" $ dynText $ fmap showt counterD
  let e = ffilter (\TickInfo{..} -> (10 - _tickInfo_n) < 0) $ updated a
  pure $ tag (current pD) e

askPatternModal :: MonadFrontBase t m => m ()
askPatternModal = mdo
  goE   <- fmap fst getPasswordModalEF
  fire  <- fmap snd getPasswordSetEF
  let redrawE = leftmost [Just <$> goE, Nothing <$ passE]
  passE <- fmap (switch . current) $ widgetHold (pure never) $ ffor redrawE $ \case
    Just i -> divClass "ask-pattern-modal" $ (fmap . fmap) ((i,) . Just) askPattern
    Nothing -> pure never
  performEvent_ $ (liftIO . fire) <$> passE

setupLoginPattern :: MonadFrontBase t m => m (Event t (Text, Password))
setupLoginPattern = divClass "setup-password" $ form $ fieldset $ mdo
  loginD <- textField PWSLogin ""
  pD <- patternSaveWidget
  e <- submitClass "button button-outline" PWSSet
  validate $ poke e $ const $ runExceptT $ do
    l  <- sampleDyn loginD
    p  <- sampleDyn pD
    check PWSEmptyLogin $ not $ T.null l
    check PWSEmptyPassword $ not $ T.null p
    pure (l,p)
#endif
