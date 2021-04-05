module Sepulcas.Elements.Input.Class(
    Wrappable(..)
  , Inputable(..)
  , LocalizedPrint(..)
  , InputStrings(..)
  ) where

import Data.Proxy
import Data.Text (Text)
import Reflex.Localize
import Sepulcas.Text
import Text.Read (readMaybe)

import qualified Data.Text as T

-- | Helper class that defines a type than can be wrapped by the second type
class Wrappable a b where
  wrap :: a -> b

-- | Describes values that can be displayed and parsed from text inputs widgets
class LocalizedPrint l => Inputable l a where
  parseInput :: Text -> Either l a
  displayInput :: Proxy l -> a -> Text

data InputStrings =
    IntParseError
  | WordParseError
  deriving (Eq, Show)

instance {-# OVERLAPPABLE #-} (Inputable l a, LocalizedPrint InputStrings) => Inputable l (Maybe a) where
  parseInput t = if T.null $ T.strip t then Right Nothing
    else Just <$> parseInput t
  {-# INLINABLE parseInput #-}
  displayInput _ = maybe "" (displayInput (Proxy :: Proxy l))
  {-# INLINABLE displayInput #-}

instance (Wrappable InputStrings l, LocalizedPrint l, LocalizedPrint InputStrings) => Inputable l Int where
  parseInput = maybe (Left $ wrap IntParseError) Right . readMaybe . T.unpack
  {-# INLINABLE parseInput #-}
  displayInput _ = showt
  {-# INLINABLE displayInput #-}
