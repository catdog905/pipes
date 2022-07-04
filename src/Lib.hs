module Lib
    ( glossExample
    ) where

import RenderingOfUniverse
import Objects
import UsefulFunctions

import           CodeWorld

initialWorld :: Int -> Int -> World
initialWorld width height= World {
  worldMap = generateCellsMatrix width height
  , playQueue = availableCells
  , debug = blank
}


glossExample :: IO ()
glossExample = activityOf (initialWorld width height) handleEvent renderWorld

handleEventNew :: Event -> World -> World
handleEventNew ev world = world

-- Simulation -----------------------------------------------------------------
updateWorld :: Float -> World -> World
updateWorld dt world = world

renderThis :: World -> Picture
renderThis world = circle 5

-- Events ---------------------------------------------------------------------
handleEvent :: Event -> World -> World
handleEvent (PointerPress (x, y)) world = updatePressedCell world cellPosition
  where
    cellPosition = Just (x, y) -- check it based on x and y
handleEvent _event world = world
