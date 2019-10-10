module Reflex.Dom.Retractable.Class(
    MonadRetract(..)
  ) where

import Reflex (Event)

-- | Defines context of widget that can switch control to next widget
-- and can be returned back with preserving of state.
class MonadRetract t m where
  -- | Switch current widget to the next widget
  nextWidget :: Event t (m ()) -> m ()
