module Objects
    ( Environment (..)
    , FluidConfig (..)
    , Universe    (..)
    , Particle    (..)
    , Solid       (..)
    , KernelFunc
    , Force
    , GateStatus  (..)
    , Cell (..)
    , World (..)
    , availableCells
    , marginScale
    , gridScale
    , upDownCell
    ) where

import Graphics.Gloss

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




data Universe = Universe
  { simulationScale :: (Float, Float)
  , environment     :: Environment
  , fluid           :: [Particle]
  , walls           :: [Solid]
  }

data Particle = Particle
  { position :: Point
  , velocity :: Vector
  , config   :: FluidConfig
  }
instance Show Particle where
  show (Particle pos vel _) = "Position: " ++ show pos ++ "   |   " ++ "Velocity: " ++ show vel
instance Eq Particle where
  (==) p1 p2 = position p1 == position p2

data Solid = Solid
  { isMovable      :: Bool
  , shape          :: Path -- | placeholder 
  , renderFunction :: Solid -> Picture
  }

data Environment = Environment
  { timeMultiplier       :: Float
  , directionOfGravity   :: Vector 
  , gravityAcceleration  :: Float 
  , densityOfEnvironment :: Float
  }

data FluidConfig = FluidConfig
  { coloring        :: Color
  , stiffness       :: Float
  , smoothingLength :: Float
  , mass            :: Float
  , viscosity       :: Float
  , surfaceTension  :: Float
  , densityKernel   :: KernelFunc
  , pressureKernel  :: KernelFunc
  , viscosityKernel :: KernelFunc
  , tensionKernel   :: KernelFunc
  }

type KernelFunc = Float -> Float -> Float

type Force = Vector

gridScale :: Float
gridScale = 0.5

marginScale :: Float
marginScale = 0.95