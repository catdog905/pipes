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
  worldMap = setAtPosition xId row (worldMap world) -- 0 for test
  , playQueue = (tailOfWorldPlayQueue world) ++ [getRandomElemFromList (fromInteger ((xId + 100 * yId) `mod` 3)) availableCells]
}
  where
    row = setAtPosition yId (headOfWorldPlayQueue world) (getElemById (fromIntegral 0) (worldMap world)) -- 0 for test

--getCellIndex :: World -> Int -> Int -> (Maybe (Int, Int))
--getCellIndex world x y =
--  where
--    shiftX :: Float
--    shiftX = gridScale * ((-86.6) * fromIntegral (length (getElemById 0 (worldMap world))))
--    shiftY :: Float
--    shiftY = gridScale * ((fromIntegral (length (worldMap world))) * (-50))

--getCentersCoords :: [[Cell]] -> Float -> [[(Float, Float)]]
--getCentersCoords [] _ = []
--getCentersCoords (row : tail) i = [getCoordsInRow row (86.6 - (i `mod` 2) * 173 - shiftX, 150 * i - shiftY)] ++ (getCentersCoords tail (i + 1))
--  where
--    getCoordsInRow :: [Cell] -> Float-> Float -> [(Float, Float)]
--    getCoordsInRow [] _ _         = []
--    getCoordsInRow (cell : t) x y = [(x, y)] ++ (getCoordsInRow (x + 173) y t)


updatePressedCell :: World -> Maybe (Double, Double) -> World
updatePressedCell world Nothing = world
updatePressedCell world (Just (xId, yId)) = setCellInWorld (floor xId) (floor yId) world
