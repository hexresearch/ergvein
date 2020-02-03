module Ergvein.Wallet.Password(
    setupLoginPassword
  , askPassword
  , askPasswordModal
  ) where

import Control.Monad.Except
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Validate

import qualified Data.Text as T

setupLoginPassword :: MonadFrontBase t m => m (Event t (Text, Password))
setupLoginPassword = divClass "setup-password" $ form $ fieldset $ do
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
