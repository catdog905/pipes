module Lib
    ( glossExample
    ) where

import RenderingOfUniverse
import Objects
import UsefulFunctions
import Data.Text

import CodeWorld

initialWorld :: Int -> Int -> World
initialWorld width height = World {
  worldMap = setXYElem (setXYElem (generateCellsMatrix width height 25) 0 0 source) sinkX sinkY sink
  , playQueue = availableCells
  , debug = blank
  , seed = 25
  , menu = Menu {
    clock = blank
    , restartButton = (scaled 30 30 (lettering (pack "restart"))) <> (colored red (solidRectangle 100 100))
  }
}

glossExample :: IO ()
glossExample = activityOf (initialWorld width height) handleEvent renderWorld

-- Events ---------------------------------------------------------------------
handleEvent :: Event -> World -> World
handleEvent (TimePassing dt) world
  | (isWatered (getXYElem (worldMap world) sinkX sinkY)) == False = updateWater world
  | otherwise = World {
    worldMap = setXYElem (setXYElem (generateCellsMatrix width height (seed world)) 0 0 source) sinkX sinkY sink
    , playQueue = availableCells
    , debug = lettering (pack "You won!!!!!")
    , seed = seed world
    , menu = Menu {
      clock = blank
      , restartButton = (scaled 30 30 (lettering (pack "restart"))) <> (colored red (solidRectangle 100 100))
    }
  }
handleEvent (PointerPress (x, y)) world = updatePressedCell world cellPosition
  where
    cellPosition = Just (x, y) -- check it based on x and y
handleEvent _event world = world


updateWater :: World -> World
updateWater world = World {
  worldMap = sinkWater source (setXYElem (eraseRowWater (worldMap world)) 0 0 source) 0 1 RightDown
  , playQueue = (playQueue world)
  , debug = (debug world)
  , menu = (menu world)
  , seed = (seed world)
}

sinkWater :: Cell -> [[Cell]] -> Int -> Int -> Direction -> [[Cell]]
sinkWater prev rows x y from
  | x < 0 || y < 0 || x >= width || y >= height = rows
  | (isWatered elem) == True = rows
  | (isWatered (getXYElem (checkCurAccessWater prev from x y elem rows) x y)) == False = rows
  | x == 0 && y == 0 = rows
  | y `mod` 2 == 0 = sinkWater elem (sinkWater elem (sinkWater elem (sinkWater elem (sinkWater elem (sinkWater elem (checkCurAccessWater prev from x y elem rows) (x - 1) y LeftUp) (x + 1) y RightUp) (x - 1) (y + 1) Down) x (y + 1) RightDown) (x - 1) (y - 1) LeftDown) x (y - 1) Up
  | y `mod` 2 == 1 = sinkWater elem (sinkWater elem (sinkWater elem (sinkWater elem (sinkWater elem (sinkWater elem (checkCurAccessWater prev from x y elem rows) (x - 1) y LeftUp) (x + 1) y RightUp) x (y + 1) Down) (x + 1) (y + 1) RightDown) x (y - 1) LeftDown) (x + 1) (y - 1) Up
  where
    elem = getXYElem rows x y

eraseWater :: Cell -> Cell
eraseWater cell = Cell {
  up = (up cell)
  , down = (down cell)
  , rightDown = (rightDown cell)
  , rightUp = (rightUp cell)
  , leftUp = (leftUp cell)
  , leftDown = (leftDown cell)
  , isWatered = False
}

eraseWaterArr :: [Cell] -> [Cell]
eraseWaterArr [] = []
eraseWaterArr (hh : tt) = (eraseWater hh) : (eraseWaterArr tt)

eraseRowWater :: [[Cell]] -> [[Cell]]
eraseRowWater [] = []
eraseRowWater (hh : tt) = (eraseWaterArr hh) : (eraseRowWater tt)

checkCurAccessWater :: Cell -> Direction -> Int -> Int -> Cell -> [[Cell]]-> [[Cell]]
checkCurAccessWater prev Up x y cell rows
  | (up cell) == Opened && (down prev) == Opened = setXYElem rows x y (sinkCell cell)
  | otherwise = rows
checkCurAccessWater prev Down x y cell rows
  | (down cell) == Opened && (up prev) == Opened = setXYElem rows x y (sinkCell cell)
  | otherwise = rows
checkCurAccessWater prev RightDown x y cell rows
  | (rightDown cell) == Opened && (leftDown prev) == Opened = setXYElem rows x y (sinkCell cell)
  | otherwise = rows
checkCurAccessWater prev RightUp x y cell rows
  | (rightUp cell) == Opened && (leftUp prev) == Opened = setXYElem rows x y (sinkCell cell)
  | otherwise = rows
checkCurAccessWater prev LeftUp x y cell rows
  | (leftUp cell) == Opened && (rightUp prev) == Opened = setXYElem rows x y (sinkCell cell)
  | otherwise = rows
checkCurAccessWater prev LeftDown x y cell rows
  | (leftDown cell) == Opened && (rightDown prev) == Opened = setXYElem rows x y (sinkCell cell)
  | otherwise = rows

sinkCell :: Cell -> Cell
sinkCell cell = Cell {
  up = (up cell)
  , down = (down cell)
  , rightDown = (rightDown cell)
  , rightUp = (rightUp cell)
  , leftUp = (leftUp cell)
  , leftDown = (leftDown cell)
  , isWatered = True
}
