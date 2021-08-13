{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.PinCode(
    pinCodeWidget
  -- , pinCodePage
  ) where

import Data.Traversable (for)

-- import Control.Monad.IO.Class
-- import Data.List (find)
-- import Data.Maybe (fromMaybe)
-- import Data.Either (fromRight)

-- import Ergvein.Aeson
-- import Ergvein.Text
-- import Ergvein.Wallet.Language
-- import Ergvein.Wallet.Localize
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Monad
import Sepulcas.Elements
-- import Ergvein.Wallet.Page.Canvas
-- import Sepulcas.Native

-- import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data NumPadBtnAction = NumPadDigit Int | NumPadBackspace deriving Show

btnContainer :: (DomBuilder t m, PostBuild t m) => m a -> m (Event t a)
btnContainer = divButton "pincode-widget-button font-bold"

-- | Draws circles indicating the number of characters entered
pinCodeDots :: MonadFrontBase t m => Int -> Dynamic t Int -> m ()
pinCodeDots totalDotsCount dotsCountD = divClass "pincode-widget-dots mb-2" $ do
  let classesD = ffor dotsCountD $ \inputLen ->
        let filledDotsCount = if inputLen > totalDotsCount
              then totalDotsCount
              else inputLen
            filledDots = replicate filledDotsCount "fas fa-circle fa-fw"
            emptyDots = replicate (totalDotsCount - filledDotsCount) "far fa-circle fa-fw"
        in filledDots ++ emptyDots
  void $ simpleList classesD (\classD -> elClassDyn "i" classD blank)

-- | Draws a numpad and returns event with pressed button
numPadWidget :: MonadFrontBase t m => m (Event t NumPadBtnAction)
numPadWidget = divClass "pincode-widget-numpad" $ do
  digitsE <- fmap concat <$> for [1,2,3] $ \rowNum -> do
    for [1,2,3] $ \colNum -> do
      let digit = colNum + (rowNum - 1) * 3
      btnE <- btnContainer $ text $ showt digit
      pure $ NumPadDigit digit <$ btnE
  (zeroE, backspaceE) <- do
    divClass "pincode-widget-button" blank
    zeroBtnE <- btnContainer $ text "0"
    backspaceBtnE <- btnContainer $ elClass "i" "fas fa-backspace fa-fw" blank
    pure (NumPadDigit 0 <$ zeroBtnE, NumPadBackspace <$ backspaceBtnE)
  pure $ leftmost $ zeroE : backspaceE : digitsE

-- pinCodePage :: MonadFrontBase t m => m ()
-- pinCodePage = wrapperSimple False $ do
--   pinD <- pinCodeWidget 6
--   pure ()

pinCodeWidget :: MonadFrontBase t m => Int -> m (Dynamic t Password)
pinCodeWidget pinCodeLength = divClass "pincode-widget" $ mdo
  divClass "pincode-widget-title mt-2" $ do
    h4 $ text "Set security PIN"
  inputD <- foldDyn (foldFunc pinCodeLength) [] actE
  pinCodeDots pinCodeLength (length <$> inputD)
  actE <- numPadWidget
  pure $ T.concat . map showt <$> inputD

foldFunc :: Int -> NumPadBtnAction -> [Int] -> [Int]
foldFunc pinCodeLength act acc = case act of
  NumPadDigit n -> if length acc >= pinCodeLength
    then acc
    else acc ++ [n]
  NumPadBackspace -> if null acc
    then acc
    else init acc
