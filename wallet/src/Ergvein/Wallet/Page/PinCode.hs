{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.PinCode(
    NumPadBtnAction(..)
  , PinCodeSetupStep(..)
  , minPinCodeLength
  , maxPinCodeLength
  , pinCodeDots
  , confirmPinCodeDots
  , numPadWidget
  , pinCodeFoldFunc
  , pinCodeAskWidget
  ) where

import Data.Traversable (for)

import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Sepulcas.Elements

import qualified Data.Text as T

data PinCodeSetupStep =
    PinCodeSetup
  | PinCodeConfirm Int -- Сontains the length of the previously entered password
  deriving (Eq, Show)

data NumPadBtnAction = NumPadDigit Int | NumPadBackspace | NumPadSubmit deriving (Eq, Show)

minPinCodeLength :: Int
minPinCodeLength = 6

maxPinCodeLength :: Int
maxPinCodeLength = 12

pinCodeDots :: MonadFrontBase t m => Dynamic t Int -> m ()
pinCodeDots dotsCountD = divClass "pincode-widget-dots" $ do
  let classesD = ffor dotsCountD $ \inputLen -> if inputLen == 0
        then replicate minPinCodeLength "material-icons-outlined"
        else replicate inputLen "material-icons-filled"
  void $ simpleList classesD (\classD -> divClass "pincode-widget-dot" $ dynMaterialIcon classD (pure "circle"))

-- | Draws circles indicating the number of characters entered
confirmPinCodeDots :: MonadFrontBase t m => Int -> Dynamic t Int -> m ()
confirmPinCodeDots totalDotsCount dotsCountD = divClass "pincode-widget-dots" $ do
  let classesD = ffor dotsCountD $ \inputLen ->
        let filledDotsCount = if inputLen > totalDotsCount
              then totalDotsCount
              else inputLen
            filledDots = replicate filledDotsCount "material-icons-filled"
            emptyDots = replicate (totalDotsCount - filledDotsCount) "material-icons-outlined"
        in filledDots ++ emptyDots
  void $ simpleList classesD (\classD -> divClass "pincode-widget-dot" $ dynMaterialIcon classD (pure "circle"))

btnContainer :: (DomBuilder t m, PostBuild t m) => m a -> m (Event t a)
btnContainer = divButton "pincode-widget-button font-bold"

-- | Draws a numpad and returns event with pressed button
numPadWidget :: MonadFrontBase t m => PinCodeSetupStep -> m (Event t NumPadBtnAction)
numPadWidget step = divClass "pincode-widget-numpad" $ do
  digitsE <- fmap concat <$> for [1,2,3] $ \rowNum -> do
    for [1,2,3] $ \colNum -> do
      let digit = colNum + (rowNum - 1) * 3
      btnE <- btnContainer $ text $ showt digit
      pure $ NumPadDigit digit <$ btnE
  (submitE, zeroE, backspaceE) <- do
    submitBtnE <- case step of
      PinCodeSetup -> btnContainer $ materialIconRound "done"
      PinCodeConfirm _ -> never <$ divClass "" blank
    zeroBtnE <- btnContainer $ text "0"
    backspaceBtnE <- btnContainer $ materialIconRound "backspace"
    pure (NumPadSubmit <$ submitBtnE, NumPadDigit 0 <$ zeroBtnE, NumPadBackspace <$ backspaceBtnE)
  pure $ leftmost $ submitE : zeroE : backspaceE : digitsE

pinCodeFoldFunc :: PinCodeSetupStep -> NumPadBtnAction -> [Int] -> [Int]
pinCodeFoldFunc step act acc = case act of
  NumPadDigit n -> if length acc >= maxLength step
    then acc
    else acc ++ [n]
  NumPadBackspace -> if null acc
    then acc
    else init acc
  NumPadSubmit -> acc
  where
    maxLength :: PinCodeSetupStep -> Int
    maxLength PinCodeSetup = maxPinCodeLength
    maxLength (PinCodeConfirm n) = n

pinCodeAskWidget :: (MonadFrontBase t m, LocalizedPrint l) => l -> m (Event t Password)
pinCodeAskWidget lbl = divClass "pincode-widget" $ mdo
  divClass "pincode-widget-title mt-2" $ do
    h4 $ localizedText lbl
  inputD <- foldDyn (pinCodeFoldFunc PinCodeSetup) [] actE
  divClass "pincode-widget-dots-wrapper mb-2" $ do
    pinCodeDots (length <$> inputD)
  actE <- numPadWidget PinCodeSetup
  let passD = T.concat . map showt <$> inputD
      submitE = ffilter (== NumPadSubmit) actE
      passE = tagPromptlyDyn passD submitE
  pure passE
