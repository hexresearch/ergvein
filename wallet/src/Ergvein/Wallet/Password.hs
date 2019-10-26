module Ergvein.Wallet.Password(
    setupPassword
  , askPassword
  ) where

import Control.Monad.Except
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Validate

import qualified Data.Text as T

setupPassword :: MonadFront t m => m (Event t (Maybe Password))
setupPassword = divClass "setup-password" $ form $ fieldset $ do
  p1D <- passFieldWithEye PWSPassword
  p2D <- passFieldWithEye PWSRepeat
  e <- submitClass "button button-outline" PWSSet
  validate $ poke e $ const $ runExceptT $ do
    p1 <- sampleDyn p1D
    p2 <- sampleDyn p2D
    check PWSNoMatch $ p1 == p2
    pure $ if T.null p1 then Nothing else Just p1

askPassword :: MonadFront t m => m (Event t Password)
askPassword = divClass "ask-password" $ form $ fieldset $ do
  pD <- passFieldWithEye PWSPassword
  e <- submitClass "button button-outline" PWSGo
  pure $ tag (current pD) e
