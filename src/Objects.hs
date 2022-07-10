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
  , isWatered :: Bool
}

data World = World {
  worldMap :: [[Cell]],
  playQueue :: [Cell],
  debug :: Picture
}

upDownCell :: Cell
upDownCell = Cell {
  up = Opened
  , down = Opened
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Closed
  , isWatered = False
}

rightLeftCell :: Cell
rightLeftCell = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Closed
  , leftDown = Opened
  , isWatered = False
}

rightUpLineCell :: Cell
rightUpLineCell = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Closed
  , leftDown = Opened
  , isWatered = False
}

leftUpLineCell :: Cell
leftUpLineCell = Cell {
  up = Closed
  , down = Opened
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Opened
  , isWatered = False
}

triple :: Cell
triple = Cell {
  up = Opened
  , down = Closed
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Opened
  , isWatered = False
}

downPair :: Cell
downPair = Cell {
  up = Closed
  , down = Opened
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Closed
  , isWatered = False
}

rightUpPair :: Cell
rightUpPair = Cell {
  up = Opened
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Closed
  , leftDown = Closed
  , isWatered = False
}

leftPair :: Cell
leftPair = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Opened
  , isWatered = False
}

leftDownPair :: Cell
leftDownPair = Cell {
  up = Closed
  , down = Opened
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Closed
  , isWatered = False
}

turnRightPair :: Cell
turnRightPair = Cell {
  up = Closed
  , down = Closed
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Closed
  , isWatered = False
}

leftRightPair :: Cell
leftRightPair = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Opened
  , leftDown = Closed
  , isWatered = False
}

source :: Cell
source = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Opened
  , isWatered = True
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