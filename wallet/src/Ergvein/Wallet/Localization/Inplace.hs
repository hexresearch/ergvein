module Ergvein.Wallet.Localization.Inplace(
    InplaceEditLbl(..)
  ) where

import Ergvein.Wallet.Language

-- | Default localization labels for `inplaceEditField`
data InplaceEditLbl er = InplaceEdit | InplaceSave | InplaceDelete | InplaceCancel | InplaceError er

instance LocalizedPrint er => LocalizedPrint (InplaceEditLbl er) where
  localizedShow l v = case l of
    English -> case v of
      InplaceEdit     -> "Edit"
      InplaceSave     -> "Save"
      InplaceDelete   -> "Delete"
      InplaceCancel   -> "Cancel"
      InplaceError er -> localizedShow English er
    Russian -> case v of
      InplaceEdit     -> "Изменить"
      InplaceSave     -> "Сохранить"
      InplaceDelete   -> "Удалить"
      InplaceCancel   -> "Отмена"
      InplaceError er -> localizedShow Russian er

instance LocalizedPrint () where
  localizedShow l v = ""
