{-# LANGUAGE OverloadedLists #-}
module Sepulcas.Elements.Markup(
    br
  , spanEl
  , spanClass
  , divClass'
  , elClassDyn
  , divClassDyn
  , spanClassDyn
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , li
  , ul
  , par
  , parClass
  , bold
  , imgClass
  , linedText
  , hyperlink
  , badge
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Unlift
import Data.Functor
import Data.Text (Text)
import Reflex.Dom
import Reflex.Localize
import Sepulcas.Elements.Button
import Sepulcas.Monad
import Sepulcas.OpenUrl

import qualified Data.Text as T

br :: DomBuilder t m => m ()
br = el "br" blank

spanEl :: DomBuilder t m => m a -> m a
spanEl = el "span"

spanClass :: DomBuilder t m => Text -> m a -> m a
spanClass = elClass "span"

elClassDyn :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Text -> m a -> m a
elClassDyn eln classD = elDynAttr eln $ do
  v <- classD
  pure [("class", v)]

divClass' :: (DomBuilder t m, PostBuild t m) => Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
divClass' = elClass' "div"

divClassDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
divClassDyn = elClassDyn "div"

spanClassDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> m a -> m a
spanClassDyn = elClassDyn "span"

h1 :: DomBuilder t m => m a -> m a
h1 = el "h1"

h2 :: DomBuilder t m => m a -> m a
h2 = el "h2"

h3 :: DomBuilder t m => m a -> m a
h3 = el "h3"

h4 :: DomBuilder t m => m a -> m a
h4 = el "h4"

h5 :: DomBuilder t m => m a -> m a
h5 = el "h5"

h6 :: DomBuilder t m => m a -> m a
h6 = el "h6"

li :: DomBuilder t m => m a -> m a
li = el "li"

ul :: DomBuilder t m => m a -> m a
ul = el "ul"

par :: DomBuilder t m => m a -> m a
par = el "p"

parClass :: DomBuilder t m => Text -> m a -> m a
parClass = elClass "p"

bold :: DomBuilder t m => m a -> m a
bold = el "b"

imgClass :: DomBuilder t m => Text -> Text -> m ()
imgClass src classVal = elAttr "img" [
    ("src", src)
  , ("class", classVal)] $ pure ()

-- | Places each line of text in seperate `dynText` widget. Caution: for fast changing
-- dynamics it can cause redraw flickering (e.x. triggering resize each time).
linedText :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t Text -> m ()
linedText textD = void $ simpleList (T.lines <$> textD) (\t -> dynText t >> br)

-- | Link with custom click handler which opens link in external browser
hyperlink :: (DomBuilder t m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadHasUI m, MonadUnliftIO (Performable m), PlatformNatives, MonadLocalized t m)
  => Dynamic t Text -> Text -> Text -> m ()
hyperlink classValD lbl url = do
  clickeE <- spanButton classValD lbl
  _ <- openOpenUrl $ url <$ clickeE
  pure ()

badge :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint lbl) => Text -> lbl -> m ()
badge classes lbl = elClass "code" classes $ dynText =<< localized lbl
