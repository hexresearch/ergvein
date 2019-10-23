module Ergvein.Wallet.Wrapper(
    wrapper
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Platform

-- | Common wrapper to all pages. Embeds back button for desktop version.
wrapper :: MonadFront t m => Bool -> m a -> m a
wrapper centered ma = container $ do
  when isDesktop $ do
    stD <- getRetractStack
    void $ dyn $ ffor stD $ \st -> if null st then pure () else backButton
  if centered then divClass "vertical-center" ma else ma

-- | Button for going back on widget history
backButton :: MonadFront t m => m ()
backButton = divClass "back-button" $ do
  e <- buttonClass "button button-clear" "< Back"
  void $ retract e
