{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.View.Common(
    ExpStatus(..)
  , TxConfirmationStatus(..)
  , TransType(..)
  , TransOutputType(..)
  , TxTime(..)
) where

import Data.Time (ZonedTime, zonedTimeToUTC)

-- Front types, should be moved to Utils
data ExpStatus = Expanded | Minified deriving (Eq, Show)

data TxConfirmationStatus = TransUncofirmed | TransUncofirmedParents | TransConfirmed deriving (Eq, Show)

data TransType = TransRefill | TransWithdraw deriving (Eq, Show)

data TransOutputType = TOSpent | TOUnspent | TOUnknown deriving (Eq, Show)

newtype TxTime = TxTime (Maybe ZonedTime) deriving (Show)

instance Eq TxTime where
  TxTime Nothing == TxTime Nothing = True
  TxTime (Just x) == TxTime (Just y) = zonedTimeToUTC x == zonedTimeToUTC y
  _ == _ = False

instance Ord TxTime where
  compare (TxTime Nothing) (TxTime Nothing) = EQ
  compare (TxTime Nothing) _ = GT
  compare _ (TxTime Nothing) = LT
  compare (TxTime (Just x)) (TxTime (Just y)) = compare (zonedTimeToUTC x) (zonedTimeToUTC y)