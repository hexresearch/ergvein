{-# LANGUAGE OverloadedLists #-}
module Sepulcas.Elements.Button(
    mkButtonDynAttr
  , mkButton 
  , buttonClass
  , buttonClassDynLabel
  , outlineButton
  , clearButton
  , spanButton
  , divButton
  , outlineTextIconButtonClass
  , outlineTextIconButton
  , outlineTextIconButtonTypeButton
  , outlineIconButtonClass
  , outlineSubmitTextIconButtonClass
  ) where

import Data.Map (Map)
import Data.Text (Text)
import Reflex.Dom
import Reflex.Localize

-- | Button with dynamic attributes
mkButtonDynAttr :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t (Map Text Text) -> m a -> m (Event t a)
mkButtonDynAttr eltp attrsD ma = do
  (e, a) <- elDynAttr' eltp attrsD ma
  return $ a <$ domEvent Click e

-- | Button with CSS classes
mkButton :: (DomBuilder t m, PostBuild t m) => Text -> Map Text Text -> Dynamic t Text -> m a -> m (Event t a)
mkButton eltp attrs classValD ma = do
  let attrsD = do
        classVal <- classValD
        pure $ attrs <> [("class", classVal)]
  mkButtonDynAttr eltp attrsD ma

-- | Button with CSS classes
buttonClass :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> lbl -> m (Event t ())
buttonClass classValD lbl = mkButton "button" [("onclick", "return false;")] classValD . dynText =<< localized lbl

-- | Button with CSS classes and dynamic label
buttonClassDynLabel :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> Dynamic t lbl -> m (Event t ())
buttonClassDynLabel classValD lblD = do
  langD <- getLanguage
  mkButton "button" [("onclick", "return false;")] classValD . dynText $ do
    l <- langD
    lbl <- lblD
    pure $ localizedShow l lbl

-- | Bright button with dark outline
outlineButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => lbl -> m (Event t ())
outlineButton = buttonClass "button button-outline"

-- | Button without border
clearButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => lbl -> m (Event t ())
clearButton = buttonClass "button button-clear"

-- | Span that acts like a button with CSS classes
spanButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> lbl -> m (Event t ())
spanButton classValD lbl = mkButton "span" [] classValD . dynText =<< localized lbl

-- | Div that acts like a button with CSS classes
divButton :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m (Event t a)
divButton = mkButton "div" []

-- outlineButton with icon from Font Awesome library
-- The first parameter is the button text
-- The second parameter is the icon class
-- Usage example:
-- >>> outlineTextIconButton CSPaste "fas fa-clipboard"
-- As a result, such an element will be created:
-- <button class="button button-outline href="return false;">
--   Scan QR code
--   <span class="button-icon-wrapper">
--     <i class="fas fa-qrcode"></i>
--   </span>
-- </button>
outlineTextIconButtonClass :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> lbl -> Text -> m (Event t ())
outlineTextIconButtonClass classValD lbl i =
  mkButton "button" [("onclick", "return false;")] (("button button-outline " <>) <$> classValD) $ do
    dynText =<< localized lbl
    elClass "span" "button-icon-wrapper" $ elClass "i" i blank

outlineTextIconButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => lbl -> Text -> m (Event t ())
outlineTextIconButton lbl i =
  mkButton "button" [("onclick", "return false;")] "button button-outline" $ do
    dynText =<< localized lbl
    elClass "span" "button-icon-wrapper" $ elClass "i" i blank

outlineTextIconButtonTypeButton :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => lbl -> Text -> m (Event t ())
outlineTextIconButtonTypeButton lbl i =
  mkButton "button" [("onclick", "return false;"), ("type", "button")] "button button-outline" $ do
    dynText =<< localized lbl
    elClass "span" "button-icon-wrapper" $ elClass "i" i blank

outlineIconButtonClass :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> Text -> m (Event t ())
outlineIconButtonClass classValD i =
  mkButton "button" [("onclick", "return false;")] (("button button-outline " <>) <$> classValD) $ elClass "i" i blank

outlineSubmitTextIconButtonClass :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl)
  => Dynamic t Text -> lbl -> Text -> m (Event t ())
outlineSubmitTextIconButtonClass classValD lbl i =
  mkButton "button" [("onclick", "return false;"), ("type", "submit")] (("button button-outline " <>) <$> classValD) $ do
    dynText =<< localized lbl
    elClass "span" "button-icon-wrapper" $ elClass "i" i blank
