{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Password(
    setupPassword
  , setupLoginPassword
  , askTextPassword
  , askPassword
  , askPasswordModal
  , setupLogin
  , setupPattern
  ) where

import Control.Monad.Except
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Localization.PatternKey
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Validate

import Reflex.Dom
import Reflex.Localize

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Time (UTCTime, getCurrentTime)
import           Control.Monad.IO.Class

setupPassword :: MonadFrontBase t m => m (Event t Password)
setupPassword = divClass "setup-password" $ form $ fieldset $ mdo
  p1D <- passFieldWithEye PWSPassword
  p2D <- passFieldWithEye PWSRepeat
  e <- submitClass "button button-outline" PWSSet
  validate $ poke e $ const $ runExceptT $ do
    p1 <- sampleDyn p1D
    p2 <- sampleDyn p2D
    check PWSNoMatch $ p1 == p2
    pure p1

setupLoginPassword :: MonadFrontBase t m => m (Event t (Text, Password))
setupLoginPassword = divClass "setup-password" $ form $ fieldset $ mdo
  loginD <- textFieldAttr PWSLogin ("placeholder" =: "my wallet name") ""
  p1D <- passFieldWithEye PWSPassword
  p2D <- passFieldWithEye PWSRepeat
  e <- submitClass "button button-outline" PWSSet
  validate $ poke e $ const $ runExceptT $ do
    p1 <- sampleDyn p1D
    p2 <- sampleDyn p2D
    l  <- sampleDyn loginD
    check PWSEmptyLogin $ not $ T.null l
    check PWSNoMatch $ p1 == p2
    pure (l,p1)

passwordHeader :: MonadFrontBase t m => m (Event t ())
passwordHeader =
  divClass "header-wrapper mb-1" $
    divClass "header header-black" $
      divButton "header-button ml-a" $
        elClass "i" "fas fa-times fa-fw" $ pure ()

askTextPassword :: (MonadFrontBase t m, LocalizedPrint l1, LocalizedPrint l2) => l1 -> l2 -> m (Event t Password)
askTextPassword title description = do
  divClass "password-ask-title" $ h4 $ localizedText title
  divClass "ask-password" $ form $ fieldset $ do
    pD <- passFieldWithEye description
    e <- submitClass "button button-outline" PWSGo
    pure $ tag (current pD) e

#ifdef ANDROID

askPassword :: MonadFrontBase t m => Text -> m (Event t Password)
askPassword name = do
  divClass "password-ask-title" $ h5 $ localizedText PKSUnlock
  divClass "password-ask-title" $ h5 $ localizedText $ PKSFor name
  divClass "ask-pattern" $ form $ fieldset $ mdo
    c <- loadCounter
    let cInt = case (Map.lookup name (patterntriesCount c)) of
          Just p -> p
          Nothing -> 0
    now <- liftIO $ getCurrentTime
    a <- (clockLossy 1 now)
    freezeD <- widgetHold (pure False) $ ffor (updated a) $ \TickInfo{..} -> do
      cS <- sampleDyn counterD
      let cdTime = if cS < 5
            then 0
            else 30 * (2 ^ (cS - 5))
      if (cdTime - _tickInfo_n) > 0
      then do
        divClass "backcounter" $ text $ "You should wait " <> (showt $ cdTime - _tickInfo_n) <> " sec"
        pure True
      else
        pure False
    pD <- patternAskWidget
    counterD <- holdDyn cInt $ poke (updated pD) $ \_ -> do
      freezeS <- sampleDyn freezeD
      cS <- sampleDyn counterD
      if freezeS
        then pure cS
        else pure $ cS + 1
    performEvent_ $ ffor (updated counterD) $ \cS -> do
      liftIO $ saveCounter $ PatternTries $ Map.insert name cS (patterntriesCount c)
      pure ()
    pure $ attachPromptlyDynWithMaybe (\freeze p -> if not freeze then Just p else Nothing) freezeD (updated pD)

askPasswordModal :: MonadFrontBase t m => m ()
askPasswordModal = mdo
  goE  <- fmap fst getPasswordModalEF
  fire <- fmap snd getPasswordSetEF
  let redrawE = leftmost [Just <$> goE, Nothing <$ passE, Nothing <$ closeE]
  valD <- widgetHold (pure (never, never)) $ ffor redrawE $ \case
    Just (i, name) -> divClass "ask-pattern-modal" $ do
      closeE <- passwordHeader
      passE <- divClass "mt-1" $ do
        h4 $ localizedText PKSAsk
        askPassword name
      let passE' = fmap ((i,) . Just) passE
      pure (passE', closeE)
    Nothing -> pure (never, never)
  let (passD, closeD) = splitDynPure valD
  let passE = switchDyn passD
  let closeE = switchDyn closeD
  performEvent_ $ (liftIO . fire) <$> passE

#else

askPassword :: MonadFrontBase t m => Text -> m (Event t Password)
askPassword name = askTextPassword PPSUnlock (PWSPassNamed name)

askPasswordModal :: MonadFrontBase t m => m ()
askPasswordModal = mdo
  goE  <- fmap fst getPasswordModalEF
  fire <- fmap snd getPasswordSetEF
  let redrawE = leftmost [Just <$> goE, Nothing <$ passE, Nothing <$ closeE]
  valD <- widgetHold (pure (never, never)) $ ffor redrawE $ \case
    Just (i, name) -> divClass "ask-password-modal" $ do
      closeE <- passwordHeader
      passE <- divClass "ask-password-modal-content" $ askPassword name
      let passE' = fmap ((i,) . Just) passE
      pure (passE', closeE)
    Nothing -> pure (never, never)
  let (passD, closeD) = splitDynPure valD
  let passE = switchDyn passD
  let closeE = switchDyn closeD
  performEvent_ $ (liftIO . fire) <$> passE
#endif

setupPattern :: MonadFrontBase t m => m (Event t Password)
setupPattern = divClass "setup-password" $ form $ fieldset $ mdo
  pD <- patternSaveWidget
  pE <- delay 0.1 $ updated pD
  validate $ poke pE $ const $ runExceptT $ do
    p <- sampleDyn pD
    check PWSEmptyPattern $ not $ T.null p
    pure p

setupLogin :: MonadFrontBase t m => m (Event t Text)
setupLogin = divClass "setup-password" $ form $ fieldset $ mdo
  loginD <- textField PWSLogin ""
  e <- submitClass "button button-outline" PWSSet
  validate $ poke e $ const $ runExceptT $ do
    l <- sampleDyn loginD
    check PWSEmptyLogin $ not $ T.null l
    pure l
