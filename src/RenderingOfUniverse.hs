module RenderingOfUniverse where

import Data.Tuple
import Objects
import UsefulFunctions

import CodeWorld
-- | Render whole Universe.
renderWorld :: World -> Picture
renderWorld world = scaled gridScale gridScale (renderQueue <> renderMap)
  where
    renderGrid :: [[Cell]] -> Double -> Picture
    renderGrid [] _ = blank
    renderGrid (row : t) n = (renderRow row) <> (translated (n * 86.6) 150 (renderGrid t (-n)))

    renderRow :: [Cell] -> Picture
    renderRow [] = blank
    renderRow (cell : t) = (renderCell cell) <> (translated 173 0 (renderRow t))

    centering :: Picture -> Picture
    centering pic = translated shiftX shiftY pic

    renderMap :: Picture
    renderMap = (centering (renderGrid (worldMap world) 1))

    shiftX :: Double
    shiftX = ((-86.6) * fromIntegral (length (getElemById 0 (worldMap world))))
    shiftY :: Double
    shiftY = ((fromIntegral (length (worldMap world))) * (-50))

    renderQueue :: Picture
    renderQueue = translated ((-43.3) * fromIntegral (length (playQueue world))) (shiftY * 2 + 200) (renderRow (playQueue world))

renderCell :: Cell -> Picture
renderCell c = scaled marginScale marginScale (base <> pipes)
  where
    base :: Picture
    base = colored grey (polygon [(0, 100), (86.6, 50), (86.6, (-50)), (0, (-100)), ((-86.6), (-50)), ((-86.6), 50)])

    pipes :: Picture
    pipes = ((circle 20) <> upPipe) <> rightUpPipe <> rightDownPipe <> downPipe <> leftDownPipe <> leftUpPipe

    upPipe :: Picture
    upPipe
      | ((up c) == Opened) = rotated 30 (polyline [(0, 0), (0, 86.6)])
      | otherwise          = blank

    downPipe :: Picture
    downPipe
      | ((down c) == Opened) = rotated 210 (polyline [(0, 0), (0, 86.6)])
      | otherwise            = blank

    rightUpPipe :: Picture
    rightUpPipe
      | ((rightUp c) == Opened) = rotated 90 (polyline [(0, 0), (0, 86.6)])
      | otherwise               = blank

    rightDownPipe :: Picture
    rightDownPipe
      | ((rightDown c) == Opened) = rotated 150 (polyline [(0, 0), (0, 86.6)])
      | otherwise                 = blank

    leftUpPipe :: Picture
    leftUpPipe
      | ((leftUp c) == Opened) = rotated 270 (polyline [(0, 0), (0, 86.6)])
      | otherwise              = blank

    leftDownPipe :: Picture
    leftDownPipe
      | ((leftDown c) == Opened) = rotated 330 (polyline [(0, 0), (0, 86.6)])
      | otherwise                = blank


