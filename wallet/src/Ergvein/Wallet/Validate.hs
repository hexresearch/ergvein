module Ergvein.Wallet.Validate (
    validate
  , validateNow
  , validateAmount
  , validateRecipient
  , VError(..)
  , Validation(..)
  ) where

import Control.Lens
import Data.Maybe
import Data.Ratio
import Data.Validation hiding (validate)
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Text.Parsec

import qualified Data.Text as T

newtype NonEmptyString = NonEmptyString String deriving (Show)
newtype PositiveRational = PositiveRational Rational deriving (Show)

data VError = MustNotBeEmpty
            | MustBeRational
            | MustBePositive
            | InvalidAddress
  deriving (Show)

instance LocalizedPrint VError where
  localizedShow l v = case l of
    English -> case v of
      MustNotBeEmpty -> "This field is required."
      MustBeRational -> "Enter a valid amount (example: 1.23)."
      MustBePositive -> "Value must be positive."
      InvalidAddress -> "Invalid address."
    Russian -> case v of
      MustNotBeEmpty -> "Заполните это поле."
      MustBeRational -> "Введите корректное значение (пример: 1.23)."
      MustBePositive -> "Значение должно быть положительным."
      InvalidAddress -> "Неверный адрес."

validateNonEmptyString :: String -> Validation [VError] NonEmptyString
validateNonEmptyString x = if x /= []
  then _Success # NonEmptyString x
  else _Failure # [MustNotBeEmpty]

validateRational :: String -> Validation [VError] Rational
validateRational x = case parse rational "" x of
  Left err -> _Failure # [MustBeRational]
  Right result -> _Success # result

validatePositiveRational :: Rational -> Validation [VError] PositiveRational
validatePositiveRational x = if x > 0
  then _Success # PositiveRational x
  else _Failure # [MustBePositive]

validateAmount :: String -> Validation [VError] Rational
validateAmount x = case validateNonEmptyString x of
  Failure errs -> _Failure # errs
  Success (NonEmptyString result) -> case validateRational result of
    Failure errs' -> _Failure # errs'
    Success result' -> case validatePositiveRational result' of
      Failure errs'' -> _Failure # errs''
      Success (PositiveRational result'') -> _Success # result''

validateAddress :: Currency -> String -> Validation [VError] EgvAddress
validateAddress currency addrStr = case egvAddrFromString currency (T.pack addrStr) of
  Nothing   -> _Failure # [InvalidAddress]
  Just addr -> _Success # addr

validateRecipient :: Currency -> String -> Validation [VError] EgvAddress
validateRecipient currency addrStr = case validateNonEmptyString addrStr of
  Failure errs -> _Failure # errs
  Success (NonEmptyString nonEmptyAddrStr) -> case validateAddress currency nonEmptyAddrStr of
    Failure errs' -> _Failure # errs'
    Success addr -> _Success # addr

-- | Helper for widget that displays error
errorWidget :: (MonadFrontBase t m, LocalizedPrint l) => l -> m ()
errorWidget = divClass "validate-error" . localizedText

-- | Print in place error message when value is `Left`
validate :: (MonadFrontBase t m, LocalizedPrint l) => Event t (Either l a) -> m (Event t a)
validate e = do
  widgetHold_ (pure ()) $ ffor e $ \case
    Left err -> errorWidget err
    _ -> pure ()
  pure $ fmapMaybe (either (const Nothing) Just) e

-- | Print in place error message for context where error is known in widget
-- building time.
validateNow :: (MonadFrontBase t m, LocalizedPrint l) => Either l a -> m (Maybe a)
validateNow ma = case ma of
  Left err -> do
    errorWidget err
    pure Nothing
  Right a -> pure $ Just a

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

readInt = read :: String -> Integer

number :: Stream s m Char => ParsecT s u m String
number = many1 digit

plus :: Stream s m Char => ParsecT s u m String
plus = char '+' *> number

minus :: Stream s m Char => ParsecT s u m String
minus = char '-' <:> number

integer :: Stream s m Char => ParsecT s u m String
integer = plus <|> minus <|> number

fractional :: Stream s m Char => ParsecT s u m String
fractional = option "" $ char '.' *> number

intFracToRational :: String -> String -> Rational
intFracToRational intStr fracStr = if null fracStr
  then int % 1
  else (int * exponent + frac * sign int) % exponent
  where exponent = 10 ^ length fracStr
        int = readInt intStr
        frac = readInt fracStr
        sign x = if x == 0 then signum 1 else signum x

rational :: Stream s m Char => ParsecT s u m Rational
rational = fmap (uncurry intFracToRational) intFrac
  where intFrac = (,) <$> integer <*> fractional <* eof