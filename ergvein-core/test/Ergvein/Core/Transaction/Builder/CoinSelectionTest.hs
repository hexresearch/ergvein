module Ergvein.Core.Transaction.Builder.CoinSelectionTest where

import Data.Word (Word64)
import Test.Tasty.Hspec (shouldBe)

import Ergvein.Core.Transaction.Builder.Btc (chooseCoins)
import Ergvein.Core.Transaction.Fee.Btc (BtcOutputType, guessTxFee)
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
    testCaseMFixedCoins :: Maybe [TestCoin],
    testCaseExpectedResult :: Either String ([TestCoin], Word64)
  }

checkTestCase :: TestCase -> IO ()
checkTestCase TestCase {..} = do
  let outTypes = testCaseChangeOutType : testCaseRecipientOutTypes
      findBetterSolution = False
  chooseCoins testCaseTarget testCaseFeeRate outTypes testCaseMFixedCoins findBetterSolution testCaseCoins `shouldBe` testCaseExpectedResult

unit_insufficientFunds1 :: IO ()
unit_insufficientFunds1 = do
  let testCase =
        TestCase
          { testCaseCoins = [],
            testCaseTarget = 1000,
            testCaseFeeRate = 1,
            testCaseRecipientOutTypes = [BtcP2WPKH],
            testCaseChangeOutType = BtcP2WPKH,
            testCaseMFixedCoins = Nothing,
            testCaseExpectedResult = Left "chooseCoins: No solution found"
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
            testCaseMFixedCoins = Nothing,
            testCaseExpectedResult = Left "chooseCoins: No solution found"
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
            testCaseMFixedCoins = Nothing,
            testCaseExpectedResult = Left "chooseCoins: No solution found"
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
            testCaseMFixedCoins = Nothing,
            testCaseExpectedResult = Left "chooseCoins: Target must be > 0"
          }
  checkTestCase testCase

unit_success :: IO ()
unit_success = do
  let
    target = 5000
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
          testCaseMFixedCoins = Nothing,
          testCaseExpectedResult = Right (solution, change)
        }
  checkTestCase testCase

-- This test will fail until the coin selection algorithm is fixed.
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
          testCaseMFixedCoins = Nothing,
          testCaseExpectedResult = Right (solution, 0)
        }
  checkTestCase testCase

unit_makesSlightlyLargerThanDustChange :: IO ()
unit_makesSlightlyLargerThanDustChange = do
  let
    -- A typical spendable segwit txout is 31 bytes big, and will
    -- need a CTxIn of at least 67 bytes to spend:
    -- so dust is a spendable txout less than
    -- 98*dustRelayFee/1000 (in satoshis).
    -- 294 satoshis at the default rate of 3000 sat/kB
    change = 294
    target = sum (coinValue <$> solution) - txFee - change
    feeRate = 1
    recipientOutTypes = [BtcP2WPKH]
    changeOutType = BtcP2WPKH
    solution = [TestCoin 2000 BtcP2WPKH]
    txFee = fromIntegral $ guessTxFee feeRate (changeOutType : recipientOutTypes) (coinType <$> solution)
    testCase =
      TestCase
      { testCaseCoins = [TestCoin 2000 BtcP2WPKH],
        testCaseTarget = target,
        testCaseFeeRate = feeRate,
        testCaseRecipientOutTypes = recipientOutTypes,
        testCaseChangeOutType = changeOutType,
        testCaseMFixedCoins = Nothing,
        testCaseExpectedResult = Right (solution, change)
      }
  checkTestCase testCase
