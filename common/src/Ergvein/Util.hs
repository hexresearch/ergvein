module Ergvein.Util(
    eitherToMaybe
  , eitherToMaybe'
  ) where

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _         = Nothing

eitherToMaybe' :: Either a b -> Maybe a
eitherToMaybe' (Left a) = Just a
eitherToMaybe' _        = Nothing
