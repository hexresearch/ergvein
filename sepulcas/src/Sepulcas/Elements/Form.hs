{-# LANGUAGE OverloadedLists #-}
module Sepulcas.Elements.Form(
    form
  , formClass
  , fieldset
  , fieldsetClass
  , label
  ) where

import Reflex.Dom
import Data.Text (Text)

form :: DomBuilder t m => m a -> m a
form = elAttr "form" [("onsubmit", "return false;")]

formClass :: DomBuilder t m => Text -> m a -> m a
formClass classVal = elAttr "form" [("onsubmit", "return false;"), ("class", classVal)]

fieldset :: DomBuilder t m => m a -> m a
fieldset = el "fieldset"

fieldsetClass :: DomBuilder t m => Text -> m a -> m a
fieldsetClass classVal = elAttr "fieldset" [("class", classVal)]

label :: DomBuilder t m => Text -> m a -> m a
label i = elAttr "label" ("for" =: i)
