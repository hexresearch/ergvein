module Ergvein.Wallet.OpenUrl(
    openOpenUrl
  ) where

import Data.Text (Text)
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native

openOpenUrl :: MonadFrontBase t m => Event t Text -> m (Event t ())
openOpenUrl e = runOnUiThread $ ffor e $ \url -> do
  openUrl url
