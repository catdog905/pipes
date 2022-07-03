module Objects
    ( GateStatus  (..)
    , Cell (..)
    , World (..)
    , availableCells
    , marginScale
    , gridScale
    , upDownCell
    ) where


data GateStatus = Opened | Closed
  deriving (Eq)

data Cell = Cell {
  up :: GateStatus
  , down :: GateStatus
  , rightDown :: GateStatus
  , rightUp :: GateStatus
  , leftUp :: GateStatus
  , leftDown :: GateStatus
}

data World = World {
  worldMap :: [[Cell]],
  playQueue :: [Cell]
}

upDownCell :: Cell
upDownCell = Cell {
  up = Opened
  , down = Opened
  , rightDown = Closed
  , rightUp = Closed
  , leftUp = Closed
  , leftDown = Closed
}

rightUpLineCell :: Cell
rightUpLineCell = Cell {
  up = Closed
  , down = Closed
  , rightDown = Closed
  , rightUp = Opened
  , leftUp = Closed
  , leftDown = Opened
}

leftUpLineCell :: Cell
leftUpLineCell = Cell {
  up = Closed
  , down = Closed
  , rightDown = Opened
  , rightUp = Closed
  , leftUp = Opened
  , leftDown = Closed
}

availableCells :: [Cell]
availableCells = [upDownCell, rightUpLineCell, leftUpLineCell]

gridScale :: Float
gridScale = 0.5

marginScale :: Float
marginScale = 0.95