{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Password(
    setupLoginPassword
  , askPassword
  , askPasswordModal
  , setupLogin
  , setupPattern
#ifdef ANDROID
  , askPattern
  , askPatternModal
#endif
  ) where

import Control.Monad.Except
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Validate

import Reflex.Dom

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Time (UTCTime, getCurrentTime)
import           Control.Monad.IO.Class

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
askPattern :: MonadFrontBase t m => Text -> m (Event t Password)
askPattern name = divClass "ask-pattern" $ form $ fieldset $ mdo
  c <- loadCounter
  let cInt = case (Map.lookup name (patterntriesCount c)) of
        Just p -> p
        Nothing -> 0
  now <- liftIO $ getCurrentTime
  a <- (clockLossy 1 now)
  freezD <- widgetHold (pure False) $ ffor (updated a) $ \TickInfo{..} -> do
    cS <- sampleDyn counterD
    let cdTime = if cS < 5
          then 0
          else 30 * (2 ^ (cS-5))
    if (cdTime - _tickInfo_n) > 0
    then do
      divClass "backcounter" $ text $ "You should wait " <>  (showt $ cdTime - _tickInfo_n) <> " sec"
      pure False
    else do
      pure (True)
  pD <- patternAskWidget
  counterD <- holdDyn cInt $ poke (updated pD) $ \_ -> do
    freezS <- sampleDyn freezD
    cS <- sampleDyn counterD
    if freezS
      then do
        pure (cS + 1)
      else pure cS
  performEvent_ $ ffor (updated counterD) $ \cS -> do
    liftIO $ saveCounter $ PatternTries $ Map.insert name cS (patterntriesCount c)
    pure ()
  pDE <- delay 0.2 (updated pD)
  let e = gate (current freezD) pDE
  pure $ tag (current pD) e

askPatternModal :: MonadFrontBase t m => m ()
askPatternModal = mdo
  goE   <- fmap fst getPasswordModalEF
  fire  <- fmap snd getPasswordSetEF
  let redrawE = leftmost [Just <$> goE, Nothing <$ passE]
  passE <- fmap (switch . current) $ widgetHold (pure never) $ ffor redrawE $ \case
    Just i -> divClass "ask-pattern-modal" $ (fmap . fmap) ((i,) . Just) $ askPattern ""
    Nothing -> pure never
  performEvent_ $ (liftIO . fire) <$> passE

#endif

setupPattern :: MonadFrontBase t m => m (Event t Password)
setupPattern = divClass "setup-password" $ form $ fieldset $ mdo
  pD <- patternSaveWidget
  validate $ poke (updated pD) $ const $ runExceptT $ do
    p  <- sampleDyn pD
    check PWSEmptyPattern $ not $ T.null p
    pure p

setupLogin :: MonadFrontBase t m => m (Event t Text)
setupLogin = divClass "setup-password" $ form $ fieldset $ mdo
  loginD <- textField PWSLogin ""
  e <- submitClass "button button-outline" PWSSet
  validate $ poke e $ const $ runExceptT $ do
    l  <- sampleDyn loginD
    check PWSEmptyLogin $ not $ T.null l
    pure l
