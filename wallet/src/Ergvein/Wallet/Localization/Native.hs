module Ergvein.Wallet.Localization.Native
  ( module Ergvein.Wallet.Native
  ) where

import Ergvein.Wallet.Native
import Ergvein.Wallet.Language

instance LocalizedPrint NativeAlerts where
  localizedShow l v = case l of
    English -> case v of
      NAFileDoesNotExist fn -> "File does not exist: " <> fn
      NAFileIsEmpty fname   -> "Empty file: " <> fname
      NADecodingError key   -> "Decoding error for key: " <> key
      NAGenericError err    -> "Native error: " <> err
    Russian -> case v of
      NAFileDoesNotExist fn -> "Файл не существует: " <> fn
      NAFileIsEmpty fname   -> "Файл пустой: " <> fname
      NADecodingError key   -> "Ошибка декодирования для: " <> key
      NAGenericError err    -> "Нативная ошибка: " <> err
