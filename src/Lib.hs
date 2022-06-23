module Lib
    ( glossExample
    ) where

import Graphics.Gloss.Interface.Pure.Game
import RenderingOfUniverse
import Graphics.Gloss
import Objects
import TimeModule
import SimulationModule
import UsefulFunctions

initialWorld :: Int -> Int -> World
initialWorld width height= World {
  worldMap = generateCellsMatrix width height
  , playQueue = availableCells
}

width :: Int
width = 10

height :: Int
height = 10

glossExample :: IO ()
glossExample = play window background fps initWorld render handleWorld updateWorld
 where
        window                  = FullScreen
        background              = blue
        fps                     = 60
        initWorld               = initialWorld width height
        render            world = renderWorld world
        handleWorld event world = handleEvent event world
        updateWorld dt    world = updateWorld dt world

-- Simulation -----------------------------------------------------------------
updateWorld :: Float -> World -> World
updateWorld dt world = world


-- Events ---------------------------------------------------------------------
handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton _) _ _ (x, y)) world = updatePressedCell world cellPosition
  where
    cellPosition = Just (0, 0)-- check it based on x and y
handleEvent _event world = world

