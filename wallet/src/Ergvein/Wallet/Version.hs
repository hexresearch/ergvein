module Ergvein.Wallet.Version(
    getVersionRaw
  ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Debug.Trace

foreign import ccall "get_binary_version_start" binaryVersionStart :: Ptr CChar

getVersionRaw :: IO String
getVersionRaw = peekCString binaryVersionStart
