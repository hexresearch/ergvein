module Ergvein.Wallet.Password(
    Password
  , setupPassword
  ) where

import Ergvein.Wallet.Monad

type Password = Text

setupPassword :: MonadFront t m => m (Event t Password)
setupPassword = pure never
