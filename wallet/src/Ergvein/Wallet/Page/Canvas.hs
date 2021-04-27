module Ergvein.Wallet.Page.Canvas(
    elementPosition
  , rawJSCall
  -- JS Raw Calls
  , drawGridT
  , drawGridBorderT
  , drawLineT
  , drawLinesT
  , drawLineZeroT
  , clearCanvasT
  , beginPathT
  , strokeStyleT
  , strokeT
  , rectZeroT
  , fillRectT
  , rectT
  , lineWidthT
  , moveToT
  , lineToT
  , arcT
  , strokeStyleCT
  , drawRndT
  , drawRoundLstT
  , drawRndHovT
  , rawGetCanvasJpeg
  -- Auxiliary types
  , ClientRect(..)
  , Square
  , DrawCommand(..)
  , Position
  , TouchState(..)
  , HoverState(..)
  , PatternTry(..)
  , GridStrokeColor(..)
  -- Canvas Type
  , CanvasOptions(..)
  , defCanvasOptions
  , createCanvas
  ) where

import Data.Aeson.Types as A
import Data.List  (find)
import Data.List.Split
import Data.Map.Strict   (fromList)
import Data.Maybe
import Language.Javascript.JSaddle hiding ((!!))

import Ergvein.Text
import Ergvein.Wallet.Monad

import qualified Data.Text as T

type Square  = (
    Double -- The x-axis coordinate of the rectangle's starting point.
  , Double -- The y-axis coordinate of the rectangle's starting point.
  , Double -- The rectangle's width. Positive values are to the right, and negative to the left.
  , Double -- The rectangle's height. Positive values are down, and negative are up.
  )
type Position = (Double, Double)

data DrawCommand = AddSquare | Clear deriving (Show, Eq)

data TouchState = Pressed | Unpressed deriving (Show, Eq)

data HoverState = Hovered | Unhovered deriving (Show, Eq)

data PatternTry = FirstTry | SecondTry | ErrorTry | Done deriving (Show, Eq)

data GridStrokeColor = GridStrokeWhite | GridStrokeBlack deriving (Show, Eq)

data CanvasOptions = CanvasOptions {
  coWidth   :: !Int
, coHeight  :: !Int
, coId      :: !Text
, coClass   :: !Text
} deriving (Show)

defCanvasOptions :: Text -> CanvasOptions
defCanvasOptions t = CanvasOptions 256 256 t "canvas"

createCanvas :: MonadFrontBase t m => CanvasOptions -> m (Element EventResult GhcjsDomSpace t)
createCanvas CanvasOptions{..} = do
  (canvasEl , _) <- elAttr' "canvas" canvasAttrs blank
  pure canvasEl
  where
    canvasAttrs = fromList
      [ ("height", showt coHeight)
      , ("width" , showt coWidth)
      , ("id"    , coId)
      , ("class" , coClass)
      ]

drawGridT :: Int -> Int -> Int -> [(Maybe Int, Square)] -> GridStrokeColor -> Text
drawGridT cW cH padding r strokeColor = (clearCanvasT fullW fullH)
                    <> beginPathT
                    <> " ctx.fillStyle = \"#FFFFFF\"; "
                    <> " ctx.fillRect(0,0," <> (showt fullW) <> "," <> (showt fullH) <> "); "
                    <> beginPathT
                    <> " ctx.fillStyle = \"#000000\"; "
                    <> (T.concat (fmap fillRects r))
                    <> strokeStyleCT strokeColorStr
                    <> strokeT
  where
    strokeColorStr = if strokeColor == GridStrokeWhite then "\"#FFFFFF\"" else "\"#000000\""
    fillRects (mN, (a,b,c,d)) = case mN of
      Just _ -> fillRectT (a + padding') (b + padding') c d
      Nothing -> rectT (a + padding') (b + padding') c d
    fullW = cW + 2 * padding
    fullH = cH + 2 * padding
    padding' = fromIntegral padding

drawGridBorderT :: Int -> Int -> [(Maybe Int, Square)] -> Text
drawGridBorderT cW cH r = (clearCanvasT cW cH)
                    <> beginPathT
                    <> (rectZeroT cW cH)
                    <> (T.concat (fmap fillRects r))
                    <> strokeStyleT
                    <> strokeT
  where
    fillRects (mN, (a,b,c,d)) = case mN of
      Just _ -> fillRectT a b c d
      Nothing -> ""
      --      Nothing -> rectT a b c d

clearCanvasT :: Int -> Int -> Text
clearCanvasT cW cH = " ctx.clearRect(0,0," <> (showt cW) <> "," <> (showt cH) <> "); "

beginPathT :: Text
beginPathT = " ctx.beginPath(); "

closePathT :: Text
closePathT = " ctx.closePath(); "

strokeStyleT :: Text
strokeStyleT = " ctx.strokeStyle = \"#000000\"; "

strokeT :: Text
strokeT = " ctx.stroke(); "

rectZeroT :: Int -> Int -> Text
rectZeroT cW cH = " ctx.rect(0,0," <> (showt cW) <> "," <> (showt cH) <> "); "

fillRectT :: Double -> Double -> Double -> Double -> Text
fillRectT a b c d = " ctx.fillRect(" <> (showt a) <> "," <> (showt b) <> "," <> (showt c) <> "," <> (showt d) <> "); "

rectT :: Double -> Double -> Double -> Double -> Text
rectT a b c d = " ctx.rect(" <> (showt a) <> "," <> (showt b) <> "," <> (showt c) <> "," <> (showt d) <> "); "

lineWidthT :: Int -> Text
lineWidthT lw = " ctx.lineWidth = " <> (showt lw) <> "; "

moveToT :: Double -> Double -> Text
moveToT mX mY = " ctx.moveTo(" <> (showt mX) <> "," <> (showt mY) <> "); "

lineToT :: Double -> Double -> Text
lineToT mX mY = " ctx.lineTo(" <> (showt mX) <> "," <> (showt mY) <> "); "

arcT :: Double -> Double -> Double -> Double -> Double -> Bool -> Text
arcT x y r start end b = " ctx.arc(" <> (showt x) <> "," <> (showt y) <> "," <> (showt r) <> "," <> (showt start) <> "," <> (showt end) <> "," <> ((T.toLower . showt) b) <> "); "

fillT :: Text
fillT = " ctx.fill(); "

strokeStyleCT :: Text -> Text
strokeStyleCT clr = " ctx.strokeStyle = " <> clr <> "; "

fillStyleCT :: Text -> Text
fillStyleCT clr = " ctx.fillStyle = " <> clr <> "; "

drawRndHovT :: Int -> Int -> [(Double,HoverState)] -> Text
drawRndHovT w h lst = T.concat $ [(clearCanvasT w h)] <> (fmap res lred)
  where
    lred = zip colorList $ fmap (\((_,hs),(a,b)) -> (a,b,hs)) $ zip lst lok
    res (col,(a,b,hs)) = beginPathT
                    <> (arcT x y r a b False)
                    <> (arcT x y (r/2) b a True)
                    <> closePathT
                    <> (lineWidthT 3)
                    <> (fillStyleCT (colHs col hs))
                    <> strokeT
                    <> fillT
    x = dw/2
    y = dh/2
    r = if (dw < dh)
      then 0.98*(dw/2)
      else 0.98*(dh/2)
    lunh = fmap fst lst
    l3 = scanl (\a b -> a+b*2*pi) tp lunh
    lok = zip (init l3) (tail l3)
    dw = fromIntegral w
    dh = fromIntegral h
    tp = -pi/2
    colHs c hov = case hov of
      Hovered -> "\"#000000\""
      Unhovered -> c
    colorList = ["\"#9e9e9e\"","\"#3e3e3e\"","\"#bebebe\""]

drawRoundLstT :: Int -> Int -> [Double] -> Text
drawRoundLstT w h lst =  T.concat $ fmap res lred
  where
    lred = zip colorList $ fmap (\(a,b) -> (tp+a*crcl,tp+a*crcl+b*crcl)) lok
    res (col,(a,b)) = beginPathT
                    <> (arcT x y r a b False)
                    <> (arcT x y (r/2) b a True)
                    <> closePathT
                    <> (lineWidthT 3)
                    <> (fillStyleCT col)
                    <> strokeT
                    <> fillT
    x = dw/2
    y = dh/2
    r = if (dw < dh)
      then 0.98*(dw/2)
      else 0.98*(dh/2)
    l3 = scanl (\a b -> a+b*2*pi) tp lst
    lok = zip (init l3) (tail l3)
    dw = fromIntegral w
    dh = fromIntegral h
    tp = -pi/2
    crcl = pi*2
    colorList = ["\"#9e9e9e\"","\"#3e3e3e\"","\"#bebebe\""]

drawRndT :: Int -> Int -> [Double] -> Text
drawRndT w h lst = drawRoundLstT w h lst

drawLineT :: Int -> Int -> Double -> Double -> Double -> Double -> (DrawCommand,(Double,Double)) -> [(Maybe Int, Square)] -> Text
drawLineT canvasW canvasH coordX coordY _ _ (a,(cntX,cntY)) r = case a of
  Clear ->  drawGridT canvasW canvasH 0 r GridStrokeBlack
  AddSquare -> (drawGridT canvasW canvasH 0 r GridStrokeBlack)
            <> (moveToT cntX cntY)
            <> (lineToT coordX coordY)
            <> strokeT
  --Save -> ""

drawLinesT :: (DrawCommand, [Maybe Int]) -> [(Int, Square)] -> Text
drawLinesT (dc, mi) z = case dc of
  AddSquare -> if ((length mi) < 2)
    then ""
    else T.concat $ fmap drawLs pointsList
      where
        (fjMi :: [Int]) = catMaybes mi
        (prepList :: [Int]) = ([head fjMi]) <> (concat (fmap (\a -> [a,a]) fjMi)) <> ([last fjMi])
        pointsList  = chunksOf 2 $ fmap (\a -> case (find (\(num,_) -> num == a ) z) of
            Just (_, (a',b,c,d)) -> (a'+c/2,b+d/2)
            Nothing -> (0,0) ) prepList
        drawLs :: [(Double, Double)] -> Text
        drawLs [(ax,ay),(bx,by)] = beginPathT
                                <> (lineWidthT 2)
                                <> (moveToT ax ay)
                                <> (lineToT bx by)
                                <> strokeT
        drawLs _ = ""
  Clear -> ""
  --Save -> ""

drawLineZeroT :: Text
drawLineZeroT = " ctx.moveTo(0,0); "

data ClientRect = ClientRect {
    crBottom :: !Double
  , crHeight :: !Double
  , crLeft   :: !Double
  , crRight  :: !Double
  , crTop    :: !Double
  , crWidth  :: !Double
  } deriving (Show)

instance FromJSON ClientRect where
  parseJSON = withObject "ClientRect" $ \o -> do
    crBottom <- o .: "bottom"
    crHeight <- o .: "height"
    crLeft   <- o .: "left"
    crRight  <- o .: "right"
    crTop    <- o .: "top"
    crWidth  <- o .: "width"
    pure ClientRect{..}

instance FromJSVal ClientRect where
  fromJSVal v = do
    av <- fromJSVal v
    case av of
      Nothing -> fail "Failed to convert ClientRect to aeson"
      Just a -> case fromJSON a of
        A.Error s -> fail $ "Failed to parse aeson ClientRect " ++ s
        A.Success b -> pure $ Just b

elementPosition :: MonadJSM m => RawElement GhcjsDomSpace -> m ClientRect
elementPosition e = liftJSM $ do
  void $ eval ("ergvein_elementPosition = function(a) { return a.getBoundingClientRect(); }" :: Text)
  jsv <- liftJSM $ jsg1 ("ergvein_elementPosition" :: Text) (toJSVal e)
  fromJSValUnchecked jsv

rawJSCall :: MonadJSM m => RawElement GhcjsDomSpace -> Text -> m ()
rawJSCall e t = liftJSM $ do
  void $ eval func
  void $ jsg1 funcName (toJSVal e)
  where
    (funcName :: Text) = "ergvein_drawgrid"
    (func :: Text) = " ergvein_drawgrid = function(cnv) { " <> " var ctx = cnv.getContext(\"2d\");" <> t <> " }"

rawGetCanvasJpeg :: MonadJSM m => RawElement GhcjsDomSpace -> CanvasOptions -> m (Maybe Text)
rawGetCanvasJpeg canvEl CanvasOptions{..} = liftJSM $ do
  void $ eval func
  fromJSVal =<< jsg1 funcName (toJSVal canvEl)
  where
    (funcName :: Text) = "ergvein_canvas_image_data"
    (func :: Text) = " ergvein_canvas_image_data = function(cnv) {"
                  <> " return cnv.toDataURL(\"image/jpeg\"); "
                  <> "}"
