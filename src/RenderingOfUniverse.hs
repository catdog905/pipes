module RenderingOfUniverse where

import Data.Tuple
import Graphics.Gloss
import Objects
import UsefulFunctions




renderWorld world = scale gridScale gridScale (renderQueue <> renderMap)
  where
    renderGrid :: [[Cell]] -> Float -> Picture
    renderGrid [] _ = blank
    renderGrid (row : t) n = (renderRow row) <> (translate (n * 86.6) 150 (renderGrid t (-n)))

    renderRow :: [Cell] -> Picture
    renderRow [] = blank
    renderRow (cell : t) = (renderCell cell) <> (translate 173 0 (renderRow t))

    centering :: Picture -> Picture
    centering pic = translate shiftX shiftY pic

    renderMap :: Picture
    renderMap = (centering (renderGrid (worldMap world) 1))

    shiftX :: Float
    shiftX = ((-86.6) * fromIntegral (length (getElemById 0 (worldMap world))))
    shiftY :: Float
    shiftY = ((fromIntegral (length (worldMap world))) * (-50))

    renderQueue :: Picture
    renderQueue = translate ((-43.3) * fromIntegral (length (playQueue world))) (shiftY * 2 + 200) (renderRow (playQueue world))

renderCell :: Cell -> Picture
renderCell c = scale marginScale marginScale (base <> pipes)
  where
    base :: Picture
    base = color (greyN 0.3) (polygon [(0, 100), (86.6, 50), (86.6, (-50)), (0, (-100)), ((-86.6), (-50)), ((-86.6), 50)])

    pipes :: Picture
    pipes = ((circle 20) <> upPipe) <> rightUpPipe <> rightDownPipe <> downPipe <> leftDownPipe <> leftUpPipe

    upPipe :: Picture
    upPipe
      | ((up c) == Opened) = rotate 30 (line [(0, 0), (0, 86.6)])
      | otherwise          = blank

    downPipe :: Picture
    downPipe
      | ((down c) == Opened) = rotate 210 (line [(0, 0), (0, 86.6)])
      | otherwise            = blank

    rightUpPipe :: Picture
    rightUpPipe
      | ((rightUp c) == Opened) = rotate 90 (line [(0, 0), (0, 86.6)])
      | otherwise               = blank

    rightDownPipe :: Picture
    rightDownPipe
      | ((rightDown c) == Opened) = rotate 150 (line [(0, 0), (0, 86.6)])
      | otherwise                 = blank

    leftUpPipe :: Picture
    leftUpPipe
      | ((leftUp c) == Opened) = rotate 270 (line [(0, 0), (0, 86.6)])
      | otherwise              = blank

    leftDownPipe :: Picture
    leftDownPipe
      | ((leftDown c) == Opened) = rotate 330 (line [(0, 0), (0, 86.6)])
      | otherwise                = blank

