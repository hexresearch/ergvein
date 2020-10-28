module Ergvein.Wallet.Elements.Input.Class(
    Wrappable(..)
  , Inputable(..)
  , LocalizedPrint(..)
  ) where

import Data.Proxy
import Data.Text (Text)
import Reflex.Localize
import Ergvein.Wallet.Localization.Input
import Ergvein.Text
import Text.Read (readMaybe)

import qualified Data.Text as T

-- | Helper class that defines a type than can be wrapped by the second type
class Wrappable a b where
  wrap :: a -> b

-- | Describes values that can be displayed and parsed from text inputs widgets
class LocalizedPrint l => Inputable l a where
  parseInput :: Text -> Either l a
  displayInput :: Proxy l -> a -> Text

instance {-# OVERLAPPABLE #-} Inputable l a => Inputable l (Maybe a) where
  parseInput t = if T.null $ T.strip t then Right Nothing
    else Just <$> parseInput t
  {-# INLINABLE parseInput #-}
  displayInput _ = maybe "" (displayInput (Proxy :: Proxy l))
  {-# INLINABLE displayInput #-}

instance (Wrappable InputStrings l, LocalizedPrint l) => Inputable l Int where
  parseInput = maybe (Left $ wrap IntParseError) Right . readMaybe . T.unpack
  {-# INLINABLE parseInput #-}
  displayInput _ = showt
  {-# INLINABLE displayInput #-}
