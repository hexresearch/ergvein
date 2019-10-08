module Ergvein.Wallet.Password(
    Password
  , setupPassword
  ) where

import Control.Monad.Except
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Validate

type Password = Text

setupPassword :: MonadFront t m => m (Event t Password)
setupPassword = divClass "setup-password" $ form $ fieldset $ do
  p1D <- passwordField "Password"
  p2D <- passwordField "Repeat password"
  e <- submitClass "button button-outline" "Set"
  validate $ poke e $ const $ runExceptT $ do
    p1 <- sampleDyn p1D
    p2 <- sampleDyn p2D
    check "Passwords must match!" $ p1 == p2
    pure p1

-- | Password field with toggleable visibility
passwordField :: MonadFront t m => Text -> m (Dynamic t Password)
passwordField _ = pure $ pure ""
