module Objects
    ( GateStatus  (..)
    , Cell (..)
    , World (..)
    , availableCells
    , marginScale
    , gridScale
    , upDownCell
    , width
    , height
    ) where

import CodeWorld

data GateStatus = Opened | Closed
  deriving (Eq)

data Cell = Cell {
  up :: GateStatus
  , down :: GateStatus
  , rightDown :: GateStatus
  , rightUp :: GateStatus
  , leftUp :: GateStatus
  , leftDown :: GateStatus
  , status :: Bool
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
  , status = False
}

rightLeftCell :: Cell
rightLeftCell = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Closed
  , leftDown = Opened
  , status = False
}

rightUpLineCell :: Cell
rightUpLineCell = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Closed
  , leftDown = Opened
  , status = False
}

leftUpLineCell :: Cell
leftUpLineCell = Cell {
  up = Closed
  , down = Closed
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Closed
  , status = False
}

source :: Cell
source = Cell {
  up = Closed
  , down = Closed
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Closed
  , status = True
}

availableCells :: [Cell]
availableCells = [upDownCell, rightUpLineCell, leftUpLineCell, rightLeftCell]

gridScale :: Double
gridScale = 0.01

marginScale :: Double
marginScale = 0.95

width :: Int
width = 10

height :: Int
height = 10