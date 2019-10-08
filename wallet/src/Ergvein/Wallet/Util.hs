module Ergvein.Wallet.Util(
    widgetHoldDyn
  , updatedWithInit
  ) where

import Reflex.Dom

-- | Same as 'widgetHold' but for dynamic
widgetHoldDyn :: forall t m a . (DomBuilder t m, MonadHold t m) => Dynamic t (m a) -> m (Dynamic t a)
widgetHoldDyn maD = do
  ma <- sample . current $ maD
  widgetHold ma $ updated maD

-- | Same as 'updated', but fires init value with 'getPostBuild'
updatedWithInit :: PostBuild t m => Dynamic t a -> m (Event t a)
updatedWithInit da = do
  buildE <- getPostBuild
  pure $ leftmost [updated da, current da `tag` buildE]
