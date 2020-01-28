{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Widget.GraphPinCode(
    ActionPinCode (..)
  , PinCode (..)
  , graphPinCode
  ) where

import qualified Data.Map.Strict as M
import Data.Function.Flip (flip3)

import Ergvein.Text
import Ergvein.Wallet.Monad

data ActionPinCode
  = CleanPinCode
  | ResetPinCode
  | ErrorPinCode
  deriving Show

data PinCode = PinCode { unPinCode :: [Int] } deriving (Eq, Show)

data PinProcess = PinProcess
  { pinProcess'flag :: Bool
  , pinProcess'pin  :: PinCode
  } deriving (Eq, Show)

data PinAct
  = PinClean
  | PinStart
  | PinStop
  | PinAdd Int
  deriving Show

isEmptyPinCode :: PinCode -> Bool
isEmptyPinCode = null . unPinCode

initPinProcess :: PinProcess
initPinProcess = PinProcess False $ PinCode []

addPinProcess :: PinProcess -> Int -> PinProcess
addPinProcess pp@PinProcess{..} v =
  let upc = unPinCode pinProcess'pin in case canAdd upc v of
    True  -> pp {pinProcess'pin = PinCode (upc ++ [v]) }
    False -> pp
  where
    canAdd :: [Int] -> Int -> Bool
    canAdd [] v =
      pinProcess'flag
    canAdd xs v =
      if last xs == v
        then False
        else pinProcess'flag

graphPinCode :: MonadFrontBase t m => Event t ActionPinCode -> m (Event t PinCode)
graphPinCode actionE = mdo
  actionD <- holdDyn CleanPinCode actionE
  (e, itemE) <- elDynAttr' "div" (canvasAttrsDyn actionD) $ do
    resE <- fmap leftmost $ mapM (\(n,x,y) -> elItem n x y) itemsGeom
    chResE <- fmap switchDyn $ divClass "graph-pin-code-glass" $
      widgetHold (pure never) $ ffor (updated pinProcessD) $ \pp -> do
        let pvs = unPinCode . pinProcess'pin $ pp
        chActE <- fmap leftmost $ mapM (\(n,x,y) -> elItemCheck pvs n x y) itemsGeom
        drawLines pvs
        pure chActE
    pure $ leftmost [resE, chResE]
  let pinActE = leftmost [ PinStart <$ domEvent Mousedown e
                         , PinStop  <$ domEvent Mouseup   e
                         , itemE
                         ]
  pinProcessD' <- flip3 foldDyn pinActE initPinProcess $
                    \act pp -> case act of
                      PinClean -> initPinProcess
                      PinStart -> initPinProcess {pinProcess'flag = True}
                      PinStop  -> pp {pinProcess'flag = False}
                      PinAdd v -> addPinProcess pp v
  pinProcessD <- holdUniqDyn pinProcessD'
  pure $ fforMaybe (updated pinProcessD) $ \PinProcess{..} ->
    if pinProcess'flag == False && isEmptyPinCode pinProcess'pin == False
      then Just pinProcess'pin
      else Nothing
  where
    itemsGeom :: [(Int,Int,Int)]
    itemsGeom = [ (1, sizeGrid  , sizeGrid  )
                , (2, sizeGrid*3, sizeGrid  )
                , (3, sizeGrid*5, sizeGrid  )
                , (4, sizeGrid  , sizeGrid*3)
                , (5, sizeGrid*3, sizeGrid*3)
                , (6, sizeGrid*5, sizeGrid*3)
                , (7, sizeGrid  , sizeGrid*5)
                , (8, sizeGrid*3, sizeGrid*5)
                , (9, sizeGrid*5, sizeGrid*5)
                ]
    canvasWidth, canvasHeight, sizeGrid :: Int
    canvasWidth = 420
    canvasHeight = 420
    sizeGrid = 420 `div` 6

    itemR :: Int
    itemR = sizeGrid `div` 4

    canvasAttrsDyn :: Reflex t => Dynamic t ActionPinCode -> Dynamic t (M.Map Text Text)
    canvasAttrsDyn actionD = ffor actionD $ \act ->
      [ ("id"   , "id_graph_pin_code_canvas")
      , ("class", case act of
                    ErrorPinCode  -> "graph-pin-code-canvas-ergvein"
                    _             -> "graph-pin-code-canvas"
        )
      , ("style", canvasStyle               )
      ]

    canvasStyle :: Text
    canvasStyle = "width:"  <> showt canvasWidth  <> "px" <> ";"
               <> "height:" <> showt canvasHeight <> "px" <> ";"

    itemAttrs :: Int -> Int -> Int -> Int -> Text -> M.Map Text Text
    itemAttrs nmb posX posY whVal pref =
      [ ("id"   , "id_graph_pin_item_" <> pref <> showt nmb)
      , ("class", "graph-pin-code-" <> pref                )
      , ("style", itemStyle posX posY whVal                )
      ]

    itemStyle :: Int -> Int -> Int -> Text
    itemStyle posX posY whVal = "left:"   <> showt (posX - whVal `div` 2) <> "px" <> ";"
                             <> "top:"    <> showt (posY - whVal `div` 2) <> "px" <> ";"
                             <> "width:"  <> showt whVal <> "px" <> ";"
                             <> "height:" <> showt whVal <> "px" <> ";"

    elItem :: MonadFrontBase t m => Int -> Int -> Int -> m (Event t PinAct)
    elItem nmb posX posY = do
      (e,_) <- elAttr' "div" (itemAttrs nmb posX posY (2*itemR) "point") blank
      let downE  = domEvent Mousedown  e
          enterE = domEvent Mouseenter e
          startE = PinStart   <$ downE
          addE   = PinAdd nmb <$ enterE
      addAtStartE <- delay 0.01 $ PinAdd nmb <$ downE
      pure $ leftmost [startE, addAtStartE, addE]

    elItemCheck :: MonadFrontBase t m => [Int] -> Int -> Int -> Int -> m (Event t PinAct)
    elItemCheck pvs nmb posX posY =
      if elem nmb pvs == True
        then do
          (e,_) <- elAttr' "div" (itemAttrs nmb posX posY (3*itemR + itemR `div` 2) "point-check") blank
          let enterE = domEvent Mouseenter e
              addE   = PinAdd nmb <$ enterE
          pure addE
        else pure never

    drawLines :: MonadFrontBase t m => [Int] -> m ()
    drawLines = \case
      []         ->
        pure ()
      (x1:x2:xs) -> do
        let posX = fst $ getPosXY (x1 - 1)
            posY = snd $ getPosXY (x1 - 1)
        case calcAngle x1 x2 of
          Just (va,dx,dy) -> elAttr "div" [ ("class","graph-pin-code-line-check")
                                          , ("style","width:"     <> showt (sizeGrid*2) <> "px"       <> ";"
                                                  <> "left:"      <> showt (posX + dx)  <> "px"       <> ";"
                                                  <> "top:"       <> showt (posY + dy)  <> "px"       <> ";"
                                                  <> "transform:" <> " rotate(" <> showt va <> "deg)" <> ";"
                                            )
                                          ] blank
          Nothing -> pure ()
        drawLines (x2:xs)
      _         ->
        pure ()
      where
        calcAngle :: Int -> Int -> Maybe (Int, Int, Int)
        calcAngle v1 v2 = case v2 - v1 of
          (1 )  -> Just ( 0 ,         0  ,         0)
          (3 )  -> Just ( 90, -sizeGrid  ,  sizeGrid)
          (-1)  -> Just ( 0 , -sizeGrid*2,         0)
          (-3)  -> Just ( 90, -sizeGrid  , -sizeGrid)
          (4 )  -> Just ( 45,         0  ,  sizeGrid)
          (-4)  -> Just ( 45, -sizeGrid*2, -sizeGrid)
          (-2)  -> Just (-45,         0  , -sizeGrid)
          (2 )  -> Just (-45, -sizeGrid*2,  sizeGrid)
          _     -> Nothing

        getPosXY :: Int -> (Int,Int)
        getPosXY nmb = let (_,x,y) = itemsGeom !! nmb in (x,y)
