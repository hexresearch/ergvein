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

import Control.Lens
import Data.Ratio
import Data.Validation hiding (validate)
import Data.Word
import Reflex.Dom
import Reflex.Network
import Reflex.Flunky
import Reflex.Localize
import Reflex.Localize.Dom
import Text.Parsec

import Sepulcas.Either
import Sepulcas.Text

import qualified Data.Text as T

newtype NonEmptyString = NonEmptyString String deriving (Show)
newtype PositiveRational = PositiveRational Rational deriving (Show)
newtype GreaterThanRational = GreaterThanRational Word64 deriving (Show)

data VError ext
  = MustNotBeEmpty
  | MustBeRational
  | MustBePositive
  | MustBeNonNegativeIntegral
  | MustBeGreaterThan Rational
  | VErrorOther ext
  deriving (Show)

validateNonEmptyString :: String -> Validation [VError e] NonEmptyString
validateNonEmptyString x = if x /= []
  then _Success # NonEmptyString x
  else _Failure # [MustNotBeEmpty]

validateRational :: String -> Validation [VError e] Rational
validateRational x = case parse rational "" x of
  Left _ -> _Failure # [MustBeRational]
  Right result -> _Success # result

validatePositiveRational :: Rational -> Validation [VError e] PositiveRational
validatePositiveRational x = if x > 0
  then _Success # PositiveRational x
  else _Failure # [MustBePositive]

validateWord64 :: String -> Validation [VError e] Word64
validateWord64 x = case parse word64 "" x of
  Left _ -> _Failure # [MustBeNonNegativeIntegral]
  Right res -> _Success # res

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
validateGreaterThan (Just y) x = if (fromIntegral x) > y
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

-- (<++>) a b = (++) <$> a <*> b
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

readInt :: String -> Integer
readInt = read

number :: Stream s m Char => ParsecT s u m String
number = many1 digit

plus :: Stream s m Char => ParsecT s u m String
plus = char '+' *> number

minus :: Stream s m Char => ParsecT s u m String
minus = char '-' <:> number

integerStr :: Stream s m Char => ParsecT s u m String
integerStr = plus <|> minus <|> number

nonNegativeIntegerStr :: Stream s m Char => ParsecT s u m String
nonNegativeIntegerStr = plus <|> number

word64 :: Stream s m Char => ParsecT s u m Word64
word64 = fmap read (nonNegativeIntegerStr <* eof)

fractionalStr :: Stream s m Char => ParsecT s u m String
fractionalStr = option "" $ char '.' *> number

intFracToRational :: String -> String -> Rational
intFracToRational intStr fracStr = if null fracStr
  then int % 1
  else (int * expon + frac * sign int) % expon
  where expon = 10 ^ length fracStr
        int = readInt intStr
        frac = readInt fracStr
        sign x = if x == 0 then signum 1 else signum x

rational :: Stream s m Char => ParsecT s u m Rational
rational = fmap (uncurry intFracToRational) intFrac
  where intFrac = (,) <$> integerStr <*> fractionalStr <* eof
