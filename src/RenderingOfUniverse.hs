module RenderingOfUniverse where

import Data.Tuple
import Objects
import UsefulFunctions

import CodeWorld
-- | Rendering the whole Universe
renderWorld :: World -> Picture
renderWorld world = scaled gridScale gridScale (gameMenu <> renderDebug <> (renderQueue (playQueue world)) <> renderMap)
  where
    renderGrid :: [[Cell]] -> Double -> Int -> Picture
    renderGrid [] _ _ = blank
    renderGrid (row : t) n y
      | y == sinkY = (renderRowWithSink row 0) <> (translated (n * 86.6) 150 (renderGrid t (-n) (sinkY + 1)))
      | otherwise  = (renderRow row) <> (translated (n * 86.6) 150 (renderGrid t (-n) (y + 1)))

    renderRowWithSink :: [Cell] -> Int -> Picture
    renderRowWithSink [] _ = blank
    renderRowWithSink (cell : t) x
      | x == sinkX = (colored red (renderCell cell)) <> (translated 173 0 (renderRowWithSink t (sinkX + 1)))
      | otherwise  = (renderCell cell) <> (translated 173 0 (renderRowWithSink t (x + 1)))

    renderDebug :: Picture
    renderDebug = translated (250 - shiftX) 0 (scaled 100 100 (debug world))

    renderRow :: [Cell] -> Picture
    renderRow [] = blank
    renderRow (cell : t) = (renderCell cell) <> (translated 173 0 (renderRow t))

    centering :: Picture -> Picture
    centering pic = translated shiftX shiftY pic

    renderMap :: Picture
    renderMap = (centering (renderGrid (worldMap world) 1 0))

    shiftX :: Double
    shiftX = (-86.6) * (fromIntegral width)
    shiftY :: Double
    shiftY = (fromIntegral height) * (-50)
    renderQueue :: [Cell] -> Picture
    renderQueue a = translated ((-43.3) * fromIntegral (length a)) (shiftY * 2 + 200) (renderRowWithRedFirstEl a)

    renderRowWithRedFirstEl :: [Cell] -> Picture
    renderRowWithRedFirstEl [] = blank
    renderRowWithRedFirstEl (a : t) = (colored red (renderCell a)) <> (translated 173.2 0 (renderRow t))


    gameMenu :: Picture
    gameMenu = (clock (menu world)) <> (translated shiftX (shiftY - 200) (restartButton (menu world)))

renderCell :: Cell -> Picture
renderCell c
  | (isWatered c) == Empty = scaled marginScale marginScale (base <> pipes)
  | (isWatered c) == Flooded = colored green (scaled marginScale marginScale (base <> pipes))
  | otherwise          =  colored blue (scaled marginScale marginScale (base <> pipes))
  where
    base :: Picture
    base = colored grey (polygon [(0, 100), (86.6, 50), (86.6, (-50)), (0, (-100)), ((-86.6), (-50)), ((-86.6), 50)])

    pipes :: Picture
    pipes = (solidCircle 19) <> upPipe <> rightUpPipe <> rightDownPipe <> downPipe <> leftDownPipe <> leftUpPipe

    upPipe :: Picture
    upPipe
      | ((up c) == Opened) = rotated (pi / 6) (thickPolyline 40 [(0, 0), (0, 87)])
      | otherwise          = blank

    downPipe :: Picture
    downPipe
      | ((down c) == Opened) = rotated (pi + pi / 6) (thickPolyline 40 [(0, 0), (0, 87)])
      | otherwise            = blank

    rightUpPipe :: Picture
    rightUpPipe
      | ((rightUp c) == Opened) = rotated (pi / 2) (thickPolyline 40 [(0, 0), (0, 87)])
      | otherwise               = blank

    rightDownPipe :: Picture
    rightDownPipe
      | ((rightDown c) == Opened) = rotated (pi - pi / 6) (thickPolyline 40 [(0, 0), (0, 87)])
      | otherwise                 = blank

    leftUpPipe :: Picture
    leftUpPipe
      | ((leftUp c) == Opened) = rotated (pi + pi / 2) (thickPolyline 40 [(0, 0), (0, 87)])
      | otherwise              = blank

    leftDownPipe :: Picture
    leftDownPipe
      | ((leftDown c) == Opened) = rotated (-(pi / 6)) (thickPolyline 40 [(0, 0), (0, 87)])
      | otherwise                = blank


