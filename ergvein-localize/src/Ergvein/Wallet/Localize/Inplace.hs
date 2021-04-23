module Ergvein.Wallet.Localize.Inplace(
    InplaceEditLbl(..)
  ) where

import Ergvein.Wallet.Language
import Sepulcas.Elements.Inplace

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
