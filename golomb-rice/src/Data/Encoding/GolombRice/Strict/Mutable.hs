module Data.Encoding.GolombRice.Strict.Mutable
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
    -- * Folding
  , Shortcut(..)
  , foldl
    -- * Converting
  , fromList
  , toList
  , fromVector
  , toVector
  , fromVectorUnboxed
  , toVectorUnboxed
  , toByteString
  , fromByteString
  )
where

import           Data.Encoding.GolombRice.Strict.Internal
import           Data.Encoding.GolombRice.Item
import           Prelude                        ( )
