module Ergvein.Lens(
    humbleFields
  , apostrophedFields
  , module X
) where

import Control.Lens
import Control.Lens.TH as X
import Data.List as List
import Data.Maybe
import Language.Haskell.TH

-- | Field rules for fields in the form @ anyprefix'fieldname @
humbleFields :: LensRules
humbleFields = defaultFieldRules & lensField .~ humbleNamer

-- | A 'FieldNamer' for 'humbleFields'.
humbleNamer :: FieldNamer
humbleNamer _ _ field = maybeToList $ do
  _      <- prefix field'
  method <- niceLens
  cls    <- classNaming
  pure (MethodName (mkName cls) (mkName method))
  where
    field' = nameBase field
    prefix xs | '\'' `List.elem` xs = Just (takeWhile (/= '\'') xs)
    prefix _                        = Nothing
    niceLens    = prefix field' <&> \n -> "_" <> drop (length n + 1) field'
    classNaming = niceLens <&> ("Has'" ++)


-- | Field rules for fields in the form @ anyprefix'fieldname @
apostrophedFields :: LensRules
apostrophedFields = defaultFieldRules & lensField .~ apostrophedNamer

-- | A 'FieldNamer' for 'apostrophedFields'.
apostrophedNamer :: FieldNamer
apostrophedNamer _ _ field = maybeToList $ do
  _      <- prefix field'
  method <- niceLens
  cls    <- classNaming
  pure (MethodName (mkName cls) (mkName method))
  where
    field' = nameBase field
    prefix xs | '\'' `List.elem` xs = Just (takeWhile (/= '\'') xs)
    prefix _                        = Nothing
    niceLens    = prefix field' <&> \n -> drop (length n + 1) field'
    classNaming = niceLens <&> ("Has_" ++)
