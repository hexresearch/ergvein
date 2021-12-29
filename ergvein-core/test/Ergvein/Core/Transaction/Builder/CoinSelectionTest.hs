module Ergvein.Core.Transaction.Builder.CoinSelectionTest where

import Data.Word (Word64)
import Test.Tasty.Hspec (shouldBe)

import Ergvein.Core.Transaction.Builder.Btc (chooseCoins, tolerance, CoinSelectionError(..))
import Ergvein.Core.Transaction.Fee.Btc (BtcOutputType, guessTxFee, getDustThresholdByOutType)
import Ergvein.Types.Address (BtcAddressType(..))
import Ergvein.Types.Utxo.Btc (Coin (..))

data TestCoin = TestCoin
  { testCoin'value :: Word64,
    testCoin'type :: BtcAddressType
  }
  deriving (Eq, Show)

instance Coin TestCoin where
  coinValue = testCoin'value
  coinType = testCoin'type

data TestCase = TestCase
  { testCaseCoins :: [TestCoin],
    testCaseTarget :: Word64,
    testCaseFeeRate :: Word64,
    testCaseRecipientOutTypes :: [BtcOutputType],
    testCaseChangeOutType :: BtcOutputType,
    testCaseFixedCoins :: [TestCoin],
    testCaseExpectedResult :: Either CoinSelectionError ([TestCoin], Maybe Word64)
  }

checkTestCase :: TestCase -> IO ()
checkTestCase TestCase {..} = do
  chooseCoins testCaseTarget testCaseFeeRate testCaseRecipientOutTypes testCaseChangeOutType testCaseFixedCoins testCaseCoins `shouldBe` testCaseExpectedResult

unit_insufficientFunds1 :: IO ()
unit_insufficientFunds1 = do
  let testCase =
        TestCase
          { testCaseCoins = [],
            testCaseTarget = 1000,
            testCaseFeeRate = 1,
            testCaseRecipientOutTypes = [BtcP2WPKH],
            testCaseChangeOutType = BtcP2WPKH,
            testCaseFixedCoins = [],
            testCaseExpectedResult = Left InsufficientFunds
          }
  checkTestCase testCase

unit_insufficientFunds2 :: IO ()
unit_insufficientFunds2 = do
  let testCase =
        TestCase
          { testCaseCoins =
              [ TestCoin 1000 BtcP2WPKH,
                TestCoin 2000 BtcP2WPKH,
                TestCoin 3000 BtcP2WPKH,
                TestCoin 4000 BtcP2WPKH
              ],
            testCaseTarget = 11000,
            testCaseFeeRate = 1,
            testCaseRecipientOutTypes = [BtcP2WPKH],
            testCaseChangeOutType = BtcP2WPKH,
            testCaseFixedCoins = [],
            testCaseExpectedResult = Left InsufficientFunds
          }
  checkTestCase testCase

unit_insufficientFunds3 :: IO ()
unit_insufficientFunds3 = do
  let
    coin = TestCoin 1000 BtcP2WPKH
    target = coinValue coin - txFee + 1
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    changeOutType = BtcP2WPKH
    txFee = fromIntegral $ guessTxFee feeRate recipientOutTypes [coinType coin]
    testCase =
        TestCase
          { testCaseCoins = [coin],
            testCaseTarget = target,
            testCaseFeeRate = feeRate,
            testCaseRecipientOutTypes = recipientOutTypes,
            testCaseChangeOutType = changeOutType,
            testCaseFixedCoins = [],
            testCaseExpectedResult = Left InsufficientFunds
          }
  checkTestCase testCase

unit_insufficientFundsAfterFees :: IO ()
unit_insufficientFundsAfterFees = do
  let testCase =
        TestCase
          { testCaseCoins = [TestCoin 2000 BtcP2WPKH],
            testCaseTarget = 2000,
            testCaseFeeRate = 1,
            testCaseRecipientOutTypes = [BtcP2WPKH],
            testCaseChangeOutType = BtcP2WPKH,
            testCaseFixedCoins = [],
            testCaseExpectedResult = Left InsufficientFunds
          }
  checkTestCase testCase

unit_zeroTarget :: IO ()
unit_zeroTarget = do
  let testCase =
        TestCase
          { testCaseCoins =
              [ TestCoin 1000 BtcP2WPKH,
                TestCoin 2000 BtcP2WPKH,
                TestCoin 3000 BtcP2WPKH,
                TestCoin 4000 BtcP2WPKH
              ],
            testCaseTarget = 0,
            testCaseFeeRate = 1,
            testCaseRecipientOutTypes = [BtcP2WPKH],
            testCaseChangeOutType = BtcP2WPKH,
            testCaseFixedCoins = [],
            testCaseExpectedResult = Left TargetMustBePositive
          }
  checkTestCase testCase

unit_success :: IO ()
unit_success = do
  let
    target = 4000
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    changeOutType = BtcP2WPKH
    solution =
      [ TestCoin 3000 BtcP2WPKH,
        TestCoin 2000 BtcP2WPKH,
        TestCoin 1000 BtcP2WPKH
      ]
    txFee = fromIntegral $ guessTxFee feeRate (changeOutType : recipientOutTypes) (coinType <$> solution)
    change =
      sum (coinValue <$> solution)
        - target
        - txFee
    testCase =
      TestCase
        { testCaseCoins =
            [ TestCoin 1000 BtcP2WPKH,
              TestCoin 2000 BtcP2WPKH,
              TestCoin 3000 BtcP2WPKH,
              TestCoin 4000 BtcP2WPKH
            ],
          testCaseTarget = target,
          testCaseFeeRate = feeRate,
          testCaseRecipientOutTypes = recipientOutTypes,
          testCaseChangeOutType = changeOutType,
          testCaseFixedCoins = [],
          testCaseExpectedResult = Right (solution, Just change)
        }
  checkTestCase testCase

unit_doesNotMakeDustChange :: IO ()
unit_doesNotMakeDustChange = do
  let
    target = sum (coinValue <$> solution) - txFee - 1
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    changeOutType = BtcP2WPKH
    solution = [TestCoin 2000 BtcP2WPKH]
    txFee = fromIntegral $ guessTxFee feeRate recipientOutTypes (coinType <$> solution)
    testCase =
      TestCase
        { testCaseCoins = [TestCoin 2000 BtcP2WPKH],
          testCaseTarget = target,
          testCaseFeeRate = feeRate,
          testCaseRecipientOutTypes = recipientOutTypes,
          testCaseChangeOutType = changeOutType,
          testCaseFixedCoins = [],
          testCaseExpectedResult = Right (solution, Nothing)
        }
  checkTestCase testCase

unit_makesSlightlyLargerThanDustChange :: IO ()
unit_makesSlightlyLargerThanDustChange = do
  let
    changeOutType = BtcP2WPKH
    change = fromIntegral $ getDustThresholdByOutType changeOutType
    target = sum (coinValue <$> solution) - txFee - change
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    solution = [TestCoin 2000 BtcP2WPKH]
    txFee = fromIntegral $ guessTxFee feeRate (changeOutType : recipientOutTypes) (coinType <$> solution)
    testCase =
      TestCase
      { testCaseCoins = [TestCoin 2000 BtcP2WPKH],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = [],
        testCaseExpectedResult = Right (solution, Just change)
      }
  checkTestCase testCase

unit_sweepWallet :: IO ()
unit_sweepWallet = do
  let
    changeOutType = BtcP2WPKH
    target = sum (coinValue <$> solution) - txFee
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    coins =
      [ TestCoin 1000 BtcP2WPKH,
        TestCoin 2000 BtcP2WPKH,
        TestCoin 3000 BtcP2WPKH,
        TestCoin 4000 BtcP2WPKH
      ]
    solution = coins
    txFee = fromIntegral $ guessTxFee feeRate recipientOutTypes (coinType <$> solution)
    testCase =
      TestCase
      { testCaseCoins = coins,
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = [],
        testCaseExpectedResult = Right (solution, Nothing)
      }
  checkTestCase testCase

unit_сoinMatchesTarget :: IO ()
unit_сoinMatchesTarget = do
  let
    changeOutType = BtcP2WPKH
    target = sum (coinValue <$> solution) - txFee
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    solution = [TestCoin 3000 BtcP2WPKH]
    txFee = fromIntegral $ guessTxFee feeRate recipientOutTypes (coinType <$> solution)
    testCase =
      TestCase
      { testCaseCoins =
        [ TestCoin 1000 BtcP2WPKH,
          TestCoin 2000 BtcP2WPKH,
          TestCoin 3000 BtcP2WPKH,
          TestCoin 4000 BtcP2WPKH
        ],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = [],
        testCaseExpectedResult = Right (solution, Nothing)
      }
  checkTestCase testCase

unit_sumOfAllCoinsLessThanTargetMatchesTarget :: IO ()
unit_sumOfAllCoinsLessThanTargetMatchesTarget = do
  let
    changeOutType = BtcP2WPKH
    target = sum (coinValue <$> solution) - txFee
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    solution = [TestCoin 1000 BtcP2WPKH, TestCoin 2000 BtcP2WPKH]
    txFee = fromIntegral $ guessTxFee feeRate recipientOutTypes (coinType <$> solution)
    testCase =
      TestCase
      { testCaseCoins =
        [ TestCoin 4000 BtcP2WPKH,
          TestCoin 1000 BtcP2WPKH,
          TestCoin 3500 BtcP2WPKH,
          TestCoin 2000 BtcP2WPKH
        ],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = [],
        testCaseExpectedResult = Right (solution, Nothing)
      }
  checkTestCase testCase

unit_tolerance_сoinMatchesTarget :: IO ()
unit_tolerance_сoinMatchesTarget = do
  let
    changeOutType = BtcP2WPKH
    target = sum (coinValue <$> solution) - txFee - tolerance
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    solution = [TestCoin 3000 BtcP2WPKH]
    txFee = fromIntegral $ guessTxFee feeRate recipientOutTypes (coinType <$> solution)
    testCase =
      TestCase
      { testCaseCoins =
        [ TestCoin 1000 BtcP2WPKH,
          TestCoin 2000 BtcP2WPKH,
          TestCoin 3050 BtcP2WPKH,
          TestCoin 3000 BtcP2WPKH,
          TestCoin 4000 BtcP2WPKH
        ],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = [],
        testCaseExpectedResult = Right (solution, Nothing)
      }
  checkTestCase testCase

unit_tolerance_сoinDoesNotMatchTarget :: IO ()
unit_tolerance_сoinDoesNotMatchTarget = do
  let
    changeOutType = BtcP2WPKH
    target = 4000 - txFeeWtihoutChange - tolerance - 1
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    solution = [TestCoin 3000 BtcP2WPKH, TestCoin 2000 BtcP2WPKH, TestCoin 1000 BtcP2WPKH]
    txFeeWtihoutChange = fromIntegral $ guessTxFee feeRate recipientOutTypes [BtcP2WPKH]
    txFeeWtihChange = fromIntegral $ guessTxFee feeRate (changeOutType : recipientOutTypes) (coinType <$> solution)
    change =
      sum (coinValue <$> solution)
        - target
        - txFeeWtihChange
    testCase =
      TestCase
      { testCaseCoins =
        [ TestCoin 1000 BtcP2WPKH,
          TestCoin 2000 BtcP2WPKH,
          TestCoin 3000 BtcP2WPKH,
          TestCoin 4000 BtcP2WPKH
        ],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = [],
        testCaseExpectedResult = Right (solution, Just change)
      }
  checkTestCase testCase

unit_fixedCoins_noAdditionalInputRequired :: IO ()
unit_fixedCoins_noAdditionalInputRequired = do
  let
    changeOutType = BtcP2WPKH
    target = 1000
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    fixedCoins = [TestCoin 2000 BtcP2WPKH]
    solution = fixedCoins
    txFee = fromIntegral $ guessTxFee feeRate (changeOutType : recipientOutTypes) (coinType <$> solution)
    change =
      sum (coinValue <$> solution)
        - target
        - txFee
    testCase =
      TestCase
      { testCaseCoins =
        [ TestCoin 1000 BtcP2WPKH],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = fixedCoins,
        testCaseExpectedResult = Right (solution, Just change)
      }
  checkTestCase testCase

unit_fixedCoins_additionalInputRequired :: IO ()
unit_fixedCoins_additionalInputRequired = do
  let
    changeOutType = BtcP2WPKH
    target = 2100
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    fixedCoins = [TestCoin 2000 BtcP2WPKH]
    solution = TestCoin 1000 BtcP2WPKH : fixedCoins
    txFee = fromIntegral $ guessTxFee feeRate (changeOutType : recipientOutTypes) (coinType <$> solution)
    change =
      sum (coinValue <$> solution)
        - target
        - txFee
    testCase =
      TestCase
      { testCaseCoins =
        [TestCoin 1000 BtcP2WPKH],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = fixedCoins,
        testCaseExpectedResult = Right (solution, Just change)
      }
  checkTestCase testCase

unit_fixedCoins_сoinMatchesTarget :: IO ()
unit_fixedCoins_сoinMatchesTarget = do
  let
    changeOutType = BtcP2WPKH
    target = sum (coinValue <$> solution) - txFee
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    fixedCoins = [TestCoin 2000 BtcP2WPKH]
    solution = TestCoin 1000 BtcP2WPKH : fixedCoins
    txFee = fromIntegral $ guessTxFee feeRate recipientOutTypes (coinType <$> solution)
    testCase =
      TestCase
      { testCaseCoins = [
          TestCoin 2000 BtcP2WPKH,
          TestCoin 3000 BtcP2WPKH,
          TestCoin 1000 BtcP2WPKH,
          TestCoin 4000 BtcP2WPKH
        ],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = fixedCoins,
        testCaseExpectedResult = Right (solution, Nothing)
      }
  checkTestCase testCase

unit_fixedCoins_sumOfAllCoinsLessThanTargetMatchesTarget :: IO ()
unit_fixedCoins_sumOfAllCoinsLessThanTargetMatchesTarget = do
  let
    changeOutType = BtcP2WPKH
    target = sum (coinValue <$> solution) - txFee
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    fixedCoins = [TestCoin 2500 BtcP2WPKH]
    solution = fixedCoins ++ [TestCoin 2000 BtcP2WPKH, TestCoin 3000 BtcP2WPKH, TestCoin 1000 BtcP2WPKH]
    txFee = fromIntegral $ guessTxFee feeRate recipientOutTypes (coinType <$> solution)
    testCase =
      TestCase
      { testCaseCoins = [
          TestCoin 2000 BtcP2WPKH,
          TestCoin 3000 BtcP2WPKH,
          TestCoin 10000 BtcP2WPKH,
          TestCoin 1000 BtcP2WPKH
        ],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseFixedCoins = fixedCoins,
        testCaseExpectedResult = Right (solution, Nothing)
      }
  checkTestCase testCase
