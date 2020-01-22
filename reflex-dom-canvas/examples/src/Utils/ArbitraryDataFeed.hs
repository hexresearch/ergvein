{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
module Utils.ArbitraryDataFeed where

import           Control.Lens         (both, cons, makeLenses, makePrisms, over,
                                       to, uncons, unsnoc, (%~), (+~), (.~),
                                       (^.), _1, _2)

import           Control.Monad.Fix    (MonadFix)

import           System.Random        (StdGen)
import qualified System.Random        as Rnd

import           Data.Function        ((&))

import           Data.Text            (Text)

import           Data.Sequence        (Seq)
import qualified Data.Sequence        as S

import           Reflex               (Dynamic, Event, MonadHold, Reflex, (<@))
import qualified Reflex               as R

import           Reflex.Dom.Canvas.Context2D (CanvasM)
import qualified Reflex.Dom.Canvas.Context2D as CanvasF

dFloatFeed
  :: ( Reflex t
     , MonadHold t m
     , MonadFix m
     )
  => (Float, Float)
  -> StdGen
  -> Event t ()
  -> m (Dynamic t Float)
dFloatFeed lohi stdGen eTickInfo =
  let
    rr = Rnd.randomR lohi
  in mdo
    dRGen <- R.holdDyn (rr stdGen)
      $ (rr . snd) <$> R.current dRGen <@ eTickInfo

    pure ( fst <$> dRGen )

keepFixedSeq
  :: Int
  -> a
  -> Seq a
  -> Seq a
keepFixedSeq limit n s
  | S.length s + 1 >= limit = maybe mempty (cons n . fst) $ unsnoc s
  | otherwise               = cons n s

data Point = Point Float Float
  deriving (Show)
makePrisms ''Point

data Line = Line
  { _line_start       :: Point
  , _line_end         :: Point
  , _line_strokeStyle :: Text
  }
  deriving (Show)
makeLenses ''Line

data DataSet = DataSet
  { _dataSet_max   :: Float
  , _dataSet_min   :: Float
  , _dataSet_lines :: Seq Line
  }
  deriving (Show)
makeLenses ''DataSet

lineInstruction
  :: Line
  -> CanvasM ()
lineInstruction line = do
  let
    sigh :: (Float,Float) -> (Double,Double)
    sigh = over both ( fromRational . toRational )

    f l g =
      uncurry g (line ^. l . _Point . to sigh)

  f line_start CanvasF.moveToF
  f line_end CanvasF.lineToF

zeroLine
  :: Line
zeroLine =
  let
    zeroPoint = Point 0.0 0.0
  in
    Line zeroPoint zeroPoint "#000000"

emptyDataSet
  :: DataSet
emptyDataSet = DataSet 0.0 0.0
  ( S.singleton zeroLine )

dDataz
  :: ( Reflex t
     , MonadHold t m
     , MonadFix m
     )
  => Int
  -> Int
  -> Int
  -> Event t Float
  -> m (Dynamic t DataSet)
dDataz _ w limit eNewDataPoint =
  let
    stepToTheRight = fromIntegral $ w `div` limit
  in
    R.foldDyn
    (\n ds -> ds
      & dataSet_max %~ max n
      & dataSet_min %~ min n
      & dataSet_lines . traverse %~
      ( ( line_end . _Point . _1 +~ stepToTheRight )
      . ( line_start . _Point . _1 +~ stepToTheRight )
      )
      & dataSet_lines %~ (\xs -> addNewDataPoint n xs $ uncons xs)
    )
    emptyDataSet
    eNewDataPoint
  where
    newLine p l = l
      & line_end .~ (l ^. line_start)
      & line_start . _Point . _2 .~ p
      & line_start . _Point . _1 .~ 0.0

    addNewDataPoint n s  Nothing       = keepFixedSeq limit (newLine n zeroLine) s
    addNewDataPoint n _ (Just (h', t)) = keepFixedSeq limit (newLine n h') (cons h' t)
