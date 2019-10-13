module Ergvein.Wallet.Validate(
    validate
  , validateNow
  ) where

import Ergvein.Wallet.Monad

-- | Helper for widget that displays error
errorWidget :: MonadFront t m => Text -> m ()
errorWidget = divClass "validate-error" . text

-- | Print in place error message when value is `Left`
validate :: MonadFront t m => Event t (Either Text a) -> m (Event t a)
validate e = do
  widgetHold_ (pure ()) $ ffor e $ \case
    Left err -> errorWidget err
    _ -> pure ()
  pure $ fmapMaybe (either (const Nothing) Just) e

-- | Print in place error message for context where error is known in widget
-- building time.
validateNow :: MonadFront t m => Either Text a -> m (Maybe a)
validateNow ma = case ma of
  Left err -> do
    errorWidget err
    pure Nothing
  Right a -> pure $ Just a
