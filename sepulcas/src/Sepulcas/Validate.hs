{-# OPTIONS_GHC -Wall #-}

module Sepulcas.Validate (
    validateEvent
  , validateNow
  , validateNonEmptyText
  , validateRational
  , validateWord64
  , validateInt
  , validateGreaterThan
  , NonEmptyText(..)
  , LargeEnoughValue(..)
  , ValidationError(..)
  , Validation(..)
  , Validate(..)
  , module Data.Validation
  ) where

import Control.Lens ((#))
import Data.Text (Text)
import Data.Text.Read (rational)
import Data.Validation hiding (Validate(..), validate)
import Data.Word (Word64)
import GHC.Natural (Natural)
import Reflex.Dom
import Reflex.Localize
import Reflex.Localize.Dom
import Text.Read (readMaybe)

import Reflex.Flunky (networkHold_)
import Sepulcas.Either (justRight)

import qualified Data.Text as T

class Validate a where
  validate :: Text -> Validation [ValidationError] a

newtype NonEmptyText = NonEmptyText Text deriving (Show)
newtype LargeEnoughValue a = LargeEnoughValue a deriving (Show)

data ValidationError
  = MustNotBeEmpty
  | MustBeRational
  | MustBeNatural
  | MustBeInteger
  | MustBeGreaterThan Text
  | InvalidAddress
  | InvalidIP
  | SendAllErr
  deriving (Show)

validateNonEmptyText :: Text -> Validation [ValidationError] NonEmptyText
validateNonEmptyText x = if not $ T.null x
  then _Success # NonEmptyText x
  else _Failure # [MustNotBeEmpty]

validateRational :: Text -> Validation [ValidationError] Rational
validateRational x = case rational x of
  Right (result, "") -> _Success # result
  _ -> _Failure # [MustBeRational]

validateWord64 :: Text -> Validation [ValidationError] Word64
validateWord64 x = case readMaybe (T.unpack x) :: Maybe Natural of
  Nothing -> _Failure # [MustBeNatural]
  Just res -> _Success # fromIntegral res

validateInt :: Text -> Validation [ValidationError] Int
validateInt x = case readMaybe (T.unpack x) :: Maybe Int of
  Nothing -> _Failure # [MustBeInteger]
  Just res -> _Success # res

validateGreaterThan :: Ord a => a -> a -> (a -> Text) -> Validation [ValidationError] (LargeEnoughValue a)
validateGreaterThan x y printer = if x > y
  then _Success # LargeEnoughValue x
  else _Failure # [MustBeGreaterThan $ printer y]

-- | Helper for widget that displays error
errorWidget :: (DomBuilder t m, MonadLocalized t m, PostBuild t m, LocalizedPrint l) => l -> m ()
errorWidget = divClass "validate-error" . localizedText

-- | Print in place error message when value is `Left`
validateEvent :: (DomBuilder t m, MonadLocalized t m, PostBuild t m, MonadHold t m, LocalizedPrint l) => Event t (Either l a) -> m (Event t a)
validateEvent e = do
  networkHold_ (pure ()) $ ffor e $ \case
    Left err -> errorWidget err
    _ -> pure ()
  pure $ fmapMaybe justRight e

-- | Print in place error message for context where error is known in widget
-- building time.
validateNow :: (DomBuilder t m, MonadLocalized t m, PostBuild t m, LocalizedPrint l) => Either l a -> m (Maybe a)
validateNow ma = case ma of
  Left err -> do
    errorWidget err
    pure Nothing
  Right a -> pure $ Just a
