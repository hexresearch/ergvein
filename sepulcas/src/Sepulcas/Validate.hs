{-# OPTIONS_GHC -Wall #-}

module Sepulcas.Validate (
    toEither
  , validate
  , validateNow
  , validateAmount
  , validateNonEmptyString
  , validateRational
  , validatePositiveRational
  , validateWord64
  , validateGreaterThan
  , NonEmptyString(..)
  , PositiveRational(..)
  , GreaterThanRational(..)
  , VError(..)
  , Validation(..)
  ) where

import Control.Lens ((#))
import Data.Ratio ()
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
newtype PositiveRational = PositiveRational Rational deriving (Show)
newtype GreaterThanRational = GreaterThanRational Word64 deriving (Show)

data VError ext
  = MustNotBeEmpty
  | MustBeRational
  | MustBePositive
  | MustBeNatural
  | MustBeGreaterThan Rational
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

validatePositiveRational :: Rational -> Validation [VError e] PositiveRational
validatePositiveRational x = if x > 0
  then _Success # PositiveRational x
  else _Failure # [MustBePositive]

validateWord64 :: String -> Validation [VError e] Word64
validateWord64 x = case readMaybe x :: Maybe Natural of
  Nothing -> _Failure # [MustBeNatural]
  Just res -> _Success # fromIntegral res

validateAmount :: String -> Validation [VError e] Rational
validateAmount x = case validateNonEmptyString x of
  Failure errs -> _Failure # errs
  Success (NonEmptyString result) -> case validateRational result of
    Failure errs' -> _Failure # errs'
    Success result' -> case validatePositiveRational result' of
      Failure errs'' -> _Failure # errs''
      Success (PositiveRational result'') -> _Success # result''

validateGreaterThan :: Maybe Rational -> Word64 -> Validation [VError e] GreaterThanRational
validateGreaterThan Nothing x = _Success # GreaterThanRational x
validateGreaterThan (Just y) x = if fromIntegral x > y
  then _Success # GreaterThanRational x
  else _Failure # [MustBeGreaterThan y]

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
