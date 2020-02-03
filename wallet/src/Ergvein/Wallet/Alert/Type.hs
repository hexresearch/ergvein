module Ergvein.Wallet.Alert.Type
  (
    ErgveinAlert(..)
  ) where

import Ergvein.Wallet.Language

data ErgveinAlert
  = DebugPanicAlert
  deriving (Eq)

instance LocalizedPrint ErgveinAlert where
  localizedShow l v = case l of
    English -> case v of
      DebugPanicAlert -> "This is a test panic. Don't panic."
    Russian -> localizedShow English v
