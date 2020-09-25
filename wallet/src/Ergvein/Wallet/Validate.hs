module Ergvein.Wallet.Validate (
    validate
  , validateNow
  , validateAmount
  , validateRecipient
  , validateBtcWithUnits
  , VError(..)
  , Validation(..)
  ) where

import Control.Lens
import Data.Ratio
import Data.Validation hiding (validate)
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Text.Parsec
import Data.Word

import qualified Data.Text as T

newtype NonEmptyString = NonEmptyString String deriving (Show)
newtype PositiveRational = PositiveRational Rational deriving (Show)
newtype PositiveWord64 = PositiveWord64 Word64 deriving (Show)

data VError = MustNotBeEmpty
            | MustBeRational
            | MustBePositive
            | MustBeIntegral
            | InvalidAddress
  deriving (Show)

instance LocalizedPrint VError where
  localizedShow l v = case l of
    English -> case v of
      MustNotBeEmpty -> "This field is required."
      MustBeRational -> "Enter a valid amount (example: 1.23)."
      MustBePositive -> "Value must be positive."
      MustBeIntegral -> "Enter a valid integer"
      InvalidAddress -> "Invalid address."
    Russian -> case v of
      MustNotBeEmpty -> "Заполните это поле."
      MustBeRational -> "Введите корректное значение (пример: 1.23)."
      MustBeIntegral -> "Введите корректное целочисленное значение."
      MustBePositive -> "Значение должно быть положительным."
      InvalidAddress -> "Неверный адрес."

validateNonEmptyString :: String -> Validation [VError] NonEmptyString
validateNonEmptyString x = if x /= []
  then _Success # NonEmptyString x
  else _Failure # [MustNotBeEmpty]

validateRational :: String -> Validation [VError] Rational
validateRational x = case parse rational "" x of
  Left _ -> _Failure # [MustBeRational]
  Right result -> _Success # result

validatePositiveRational :: (Rational) -> Validation [VError] PositiveRational
validatePositiveRational x = if x > 0
  then _Success # PositiveRational x
  else _Failure # [MustBePositive]

validatePositiveWord64 :: Word64 -> Validation [VError] PositiveWord64
validatePositiveWord64 x = if x > 0
  then _Success # PositiveWord64 x
  else _Failure # [MustBePositive]

validateWord64 :: String -> Validation [VError] Word64
validateWord64 x = case parse word64 "" x of
  Left _ -> _Failure # [MustBeIntegral]
  Right res -> _Success # res

validateAmount :: String -> Validation [VError] Rational
validateAmount x = case validateNonEmptyString x of
  Failure errs -> _Failure # errs
  Success (NonEmptyString result) -> case validateRational result of
    Failure errs' -> _Failure # errs'
    Success result' -> case validatePositiveRational result' of
      Failure errs'' -> _Failure # errs''
      Success (PositiveRational result'') -> _Success # result''

validateBtcWithUnits :: UnitBTC -> String -> Validation [VError] Word64
validateBtcWithUnits unit x = case validateNonEmptyString x of
  Failure errs -> _Failure # errs
  Success (NonEmptyString result) -> case unit of
    BtcSat -> case validateWord64 result of
      Failure errs' -> _Failure # errs'
      Success result' -> case validatePositiveWord64 result' of
        Failure errs'' -> _Failure # errs''
        Success (PositiveWord64 result'') -> _Success # result''
    _ -> case validateRational result of
      Failure errs' -> _Failure # errs'
      Success result' -> case validatePositiveRational result' of
        Failure errs'' -> _Failure # errs''
        Success (PositiveRational result'') -> _Success # (floor $ result'' * 10 ^ btcResolution unit)

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

word64 :: Stream s m Char => ParsecT s u m Word64
word64 = fmap read integerStr

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
