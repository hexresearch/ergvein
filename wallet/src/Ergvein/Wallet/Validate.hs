module Ergvein.Wallet.Validate(
    validate
  ) where

import Ergvein.Wallet.Monad

-- | Print in place error message when value is `Left`
validate :: MonadFront t m => Event t (Either Text a) -> m (Event t a)
validate e = do
  widgetHold_ (pure ()) $ fforMaybe e $ \case
    Left err -> Just $ divClass "validate-error" $ text err
    _ -> Nothing
  pure $ fmapMaybe (either (const Nothing) Just) e
