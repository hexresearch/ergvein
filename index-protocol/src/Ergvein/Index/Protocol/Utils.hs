module Ergvein.Index.Protocol.Utils where

import Control.Monad.Fail                   as MFail
import qualified Data.Attoparsec.ByteString as Parse
import qualified Data.ByteString            as BS

guardJust :: MonadFail m => String -> Maybe a -> m a
guardJust msg = maybe (MFail.fail msg) pure
{-# INLINABLE guardJust #-}

parseTillEndOfInput :: Parse.Parser a -> BS.ByteString -> Either String a
parseTillEndOfInput f = Parse.parseOnly (f <* Parse.endOfInput)