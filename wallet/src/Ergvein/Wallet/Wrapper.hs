module Ergvein.Wallet.Wrapper(
    wrapper
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad

-- | Common wrapper to all pages. Embeds back button for desktop version.
wrapper :: MonadFrontBase t m => Bool -> m a -> m a
wrapper centered ma = container $ if centered then divClass "vertical-center" ma else ma
