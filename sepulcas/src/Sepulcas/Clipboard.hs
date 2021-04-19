module Sepulcas.Clipboard(
    CopyStr(..)
  , clipboardCopy
  , clipboardPaste
  , clipboardDebug
  , copyButton
  ) where

import Control.Monad.Fix
import Data.Text (Text)
import Reflex.Dom
import Reflex.Localize
import Sepulcas.Elements
import Sepulcas.Monad
import Sepulcas.Native

-- | Clipboard control widget that allows to put text into clipboard. Returns
-- event when the text is copied with the same text.
clipboardCopy :: (PerformMain t m, PlatformNatives) => Event t Text -> m (Event t Text)
clipboardCopy e = runOnMainThread $ ffor e $ \str -> do
  copyStr str
  pure str

-- | Clipboard control widget that allows to copy text from clipboard.
clipboardPaste :: (PerformMain t m, PlatformNatives) => Event t () -> m (Event t Text)
clipboardPaste e = runOnMainThread $ ffor e $ const pasteStr

data CopyStr = StrCopy | StrPaste

-- | Debug widgete for clipboard
clipboardDebug :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, PerformMain t m, LocalizedPrint CopyStr, PlatformNatives, MonadFix m) => m ()
clipboardDebug = mdo
  tinput <- inputElement $ def & inputElementConfig_setValue .~ pastedE
  copyE <- buttonClass (pure "form__btn confirm-button") StrCopy
  pasteE <- buttonClass (pure "form__btn confirm-button") StrPaste
  let copyStrE = tag (current $ _inputElement_value tinput) copyE
  _ <- clipboardCopy copyStrE
  pastedE <- clipboardPaste pasteE
  pure ()

-- | Helper to make copying button
copyButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, PerformMain t m, LocalizedPrint CopyStr, PlatformNatives) => Dynamic t Text -> m (Event t Text)
copyButton textD = do
  butE <- buttonClass (pure "form__btn confirm-button") StrCopy
  clipboardCopy $ current textD `tag` butE
