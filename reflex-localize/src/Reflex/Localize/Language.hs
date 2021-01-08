-- |
-- Module      : Reflex.Localize.Language
-- Copyright   : (c) 2019 ATUM SOLUTIONS AG
-- License     : MIT
-- Maintainer  : ncrashed@protonmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- A signature for languages enumeration that is used in your application,
-- alows to not pass around actual language implementation in type classes.
--
module Reflex.Localize.Language(
    Language
  ) where

-- | Possible languages enumeration that is defined in your application
data family Language
