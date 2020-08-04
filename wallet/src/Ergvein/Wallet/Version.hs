module Ergvein.Wallet.Version(
    getVersionRaw
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall "get_binary_version_start" binaryVersionStart :: Ptr CChar
foreign import ccall "get_binary_version_size" binaryVersionSize :: Int

getVersionRaw :: IO String
getVersionRaw = peekCStringLen (binaryVersionStart, binaryVersionSize)
