module Data.Encoding.GolombRice.Strict
  ( -- * Types
    GolombRice
  , GolombItem(..)
    -- * Creation
  , empty
  , singleton
    -- * Quering
  , null
  , headMay
  , head
    -- * Converting
  , fromList
  , toList
  )
where

import           Data.Encoding.GolombRice.Strict.Internal
import           Data.Encoding.GolombRice.Item
import           Prelude                        ( )
