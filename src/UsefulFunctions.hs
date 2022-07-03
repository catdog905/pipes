module UsefulFunctions where

import Objects
import System.Random

getElemById :: Int -> [a] -> a
getElemById n (a:taill)
  | n >= (length (a:taill)) = head taill
getElemById 0 (a:taill) = a
getElemById id (a:taill) = getElemById (id - 1) taill

seed :: Int
seed = 25

getRandomElemFromList :: Int -> [a] -> a
getRandomElemFromList id list = getElemById (id `mod` (length list)) list

generateCellsRow :: Int -> Int -> [Cell]
generateCellsRow id 0 = []
generateCellsRow id n = (getRandomElemFromList id availableCells) : (generateCellsRow n (n-1))

generateCellsMatrix :: Int -> Int -> [[Cell]]
generateCellsMatrix width 0 = []
generateCellsMatrix width n = (generateCellsRow (width `mod` n + 34) width) : (generateCellsMatrix width (n-1))


setAtPosition :: Integer -> a -> [a] -> [a]
setAtPosition _ obj [] = [obj]
setAtPosition 0 obj (h:taill) = obj : taill
setAtPosition n obj (h:taill) = h : (setAtPosition (n - 1) obj taill)

tailOfWorldPlayQueue :: World -> [Cell]
tailOfWorldPlayQueue (World _map (h:taill)) = taill

headOfWorldPlayQueue :: World ->  Cell
headOfWorldPlayQueue (World _map (h:taill)) = h

setCellInWorld :: Integer -> Integer -> World -> World
setCellInWorld xId yId world = World {
  worldMap = setAtPosition yId row (worldMap world)
  , playQueue = (tailOfWorldPlayQueue world) ++ [getRandomElemFromList (fromInteger ((xId + 100 * yId) `mod` 3)) availableCells]
}
  where
    row = setAtPosition xId (headOfWorldPlayQueue world) (getElemById (fromIntegral yId) (worldMap world))

updatePressedCell :: World -> Maybe (Integer, Integer) -> World
updatePressedCell world Nothing = world
updatePressedCell world (Just (xId, yId)) = setCellInWorld xId yId world
