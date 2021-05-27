{-# OPTIONS_GHC -Wall #-}

module Sepulcas.Validate (
    toEither
  , validate
  , validateNow
  , validateNonEmptyString
  , validateRational
  , validateWord64
  , validateGreaterThan
  , NonEmptyString(..)
  , GreaterThan(..)
  , VError(..)
  , Validation(..)
  ) where

import Control.Lens ((#))
import Data.Text (Text)
import Data.Text.Read (rational)
import Data.Validation
  ( Validation (..),
    toEither,
    _Failure,
    _Success,
  )
import Data.Word (Word64)
import GHC.Natural (Natural)
import Reflex.Dom
  ( DomBuilder,
    MonadHold,
    PostBuild,
    Reflex (Event),
    divClass,
    ffor,
    fmapMaybe,
  )
import Reflex.Flunky (networkHold_)
import Reflex.Localize (LocalizedPrint, MonadLocalized)
import Reflex.Localize.Dom (localizedText)
import Sepulcas.Either (justRight)
import Text.Read (readMaybe)

import qualified Data.Text as T

newtype NonEmptyString = NonEmptyString String deriving (Show)
newtype GreaterThan a = GreaterThan a deriving (Show)

data VError ext
  = MustNotBeEmpty
  | MustBeRational
  | MustBeNatural
  | MustBeGreaterThan Text
  | VErrorOther ext
  deriving (Show)

validateNonEmptyString :: String -> Validation [VError e] NonEmptyString
validateNonEmptyString x = if x /= []
  then _Success # NonEmptyString x
  else _Failure # [MustNotBeEmpty]

validateRational :: String -> Validation [VError e] Rational
validateRational x = case rational $ T.pack x of
  Right (result, "") -> _Success # result
  _ -> _Failure # [MustBeRational]

validateWord64 :: String -> Validation [VError e] Word64
validateWord64 x = case readMaybe x :: Maybe Natural of
  Nothing -> _Failure # [MustBeNatural]
  Just res -> _Success # fromIntegral res

validateGreaterThan :: Ord a => a -> Maybe a -> (a -> Text) -> Validation [VError e] (GreaterThan a)
validateGreaterThan x Nothing _ = _Success # GreaterThan x
validateGreaterThan x (Just y) printer = if x > y
  then _Success # GreaterThan x
  else _Failure # [MustBeGreaterThan $ printer y]

-- | Helper for widget that displays error
errorWidget :: (DomBuilder t m, MonadLocalized t m, PostBuild t m, LocalizedPrint l) => l -> m ()
errorWidget = divClass "validate-error" . localizedText

-- | Print in place error message when value is `Left`
validate :: (DomBuilder t m, MonadLocalized t m, PostBuild t m, MonadHold t m, LocalizedPrint l) => Event t (Either l a) -> m (Event t a)
validate e = do
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
