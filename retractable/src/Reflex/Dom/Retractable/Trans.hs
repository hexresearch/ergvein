-- |
-- Module      : Reflex.Dom.Retractable.Trans
-- Copyright   : (c) 2019 ATUM SOLUTIONS AG
-- License     : MIT
-- Maintainer  : ncrashed@protonmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Plug-in implementation for `Reflex.Dom.Retractable.Class.MonadRetract` using wrapper around `Control.Monad.Reader.ReaderT`.
--
-- Example of use:
--
-- @
-- import Control.Monad
-- import Reflex.Dom
-- import Reflex.Dom.Retractable
--
-- main :: IO ()
-- main = mainWidget $ runRetract frontend
--
-- frontend :: (MonadWidget t m, MonadRetract t m) => m ()
-- frontend = void $ retractStack $ pageA 42
--
-- pageA :: (MonadWidget t m, MonadRetract t m) => Int -> m ()
-- pageA n = do
--    e <- button "Go page B"
--    void $ nextWidget $ ffor e $ const Retractable {
--        retractableNext = pageB $ n + 1
--      , retractablePrev = Just $ pure $ pageA n
--      }
--
-- pageB :: (MonadWidget t m, MonadRetract t m) => Int -> m ()
-- pageB n = do
--   e <- button "Go page A"
--   void $ nextWidget $ ffor e $ const  Retractable {
--        retractableNext = pageA $ n + 1
--      , retractablePrev = Just $ pure $ pageB n
--      }
-- @
module Reflex.Dom.Retractable.Trans(
    RetractT
  , RetractEnv
  , newRetractEnv
  , runRetractT
  , runRetract
  ) where

import Reflex.Dom.Retractable.Trans.Internal
