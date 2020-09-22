module Ergvein.Index.Protocol.Utils where

import Control.Monad.Fail as MFail

guardJust :: MonadFail m => String -> Maybe a -> m a
guardJust msg = maybe (MFail.fail msg) pure
{-# INLINABLE guardJust #-}