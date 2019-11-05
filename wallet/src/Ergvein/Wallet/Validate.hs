module Ergvein.Wallet.Validate(
    validate
  , validateNow
  ) where

import Ergvein.Wallet.Monad
import Reflex.Localize

-- | Helper for widget that displays error
errorWidget :: (MonadFrontBase t m, LocalizedPrint l) => l -> m ()
errorWidget = divClass "validate-error" . localizedText

-- | Print in place error message when value is `Left`
validate :: (MonadFrontBase t m, LocalizedPrint l) => Event t (Either l a) -> m (Event t a)
validate e = do
  widgetHold_ (pure ()) $ ffor e $ \case
    Left err -> errorWidget err
    _ -> pure ()
  pure $ fmapMaybe (either (const Nothing) Just) e

-- | Print in place error message for context where error is known in widget
-- building time.
validateNow :: (MonadFrontBase t m, LocalizedPrint l) => Either l a -> m (Maybe a)
validateNow ma = case ma of
  Left err -> do
    errorWidget err
    pure Nothing
  Right a -> pure $ Just a
