module Objects
    ( GateStatus  (..)
    , Cell (..)
    , World (..)
    , Direction (..)
    , availableCells
    , marginScale
    , gridScale
    , upDownCell
    , width
    , height
    , source
    , triple
    , Menu (..)
    , sink
    , sinkY
    , sinkX
    , Status (..)
    ) where

import CodeWorld

data GateStatus = Opened | Closed
  deriving (Eq)

data Direction = Up | Down | RightDown | RightUp | LeftUp | LeftDown

data Cell = Cell {
  up :: GateStatus
  , down :: GateStatus
  , rightDown :: GateStatus
  , rightUp :: GateStatus
  , leftUp :: GateStatus
  , leftDown :: GateStatus
  , isWatered :: Status
}

data Status = Flooded | HasWater | Empty
  deriving (Eq)

data World = World {
  worldMap :: [[Cell]],
  playQueue :: [Cell],
  debug :: Picture,
  menu :: Menu,
  seed :: Int
}

data Menu = Menu {
  restartButton :: Picture,
  clock :: Picture
}

upDownCell :: Cell
upDownCell = Cell {
  up = Opened
  , down = Opened
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Closed
  , isWatered = Empty
}

rightLeftCell :: Cell
rightLeftCell = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Closed
  , leftDown = Opened
  , isWatered = Empty
}

rightUpLineCell :: Cell
rightUpLineCell = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Closed
  , leftDown = Opened
  , isWatered = Empty
}

leftUpLineCell :: Cell
leftUpLineCell = Cell {
  up = Closed
  , down = Opened
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Opened
  , isWatered = Empty
}

triple :: Cell
triple = Cell {
  up = Opened
  , down = Closed
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Opened
  , isWatered = Empty
}

downPair :: Cell
downPair = Cell {
  up = Closed
  , down = Opened
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Closed
  , isWatered = Empty
}

rightUpPair :: Cell
rightUpPair = Cell {
  up = Opened
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Closed
  , leftDown = Closed
  , isWatered = Empty
}

leftPair :: Cell
leftPair = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Opened
  , isWatered = Empty
}

leftDownPair :: Cell
leftDownPair = Cell {
  up = Closed
  , down = Opened
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Closed
  , isWatered = Empty
}

turnRightPair :: Cell
turnRightPair = Cell {
  up = Closed
  , down = Closed
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Closed
  , isWatered = Empty
}

leftRightPair :: Cell
leftRightPair = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Opened
  , leftDown = Closed
  , isWatered = Empty
}

source :: Cell
source = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Opened
  , isWatered = HasWater
}

sink :: Cell
sink = Cell {
  up = Closed
  , down = Closed
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Closed
  , isWatered = Empty
}

availableCells :: [Cell]
availableCells = [upDownCell, rightUpLineCell, rightLeftCell, leftUpLineCell, triple, downPair, rightUpPair, leftPair, leftDownPair, turnRightPair, leftRightPair]

gridScale :: Double
gridScale = 0.01

marginScale :: Double
marginScale = 0.95

width :: Int
width = 10

height :: Int
height = 10

sinkX :: Int
sinkX = width - 1

sinkY :: Int
sinkY = height - 1