module UsefulFunctions where

import Objects
import System.Random
import Data.Fixed
import CodeWorld
import Data.Text

getElemById :: Int -> [a] -> a
getElemById n (a:taill)
  | n >= (Prelude.length (a:taill)) = Prelude.head taill
getElemById 0 (a:taill) = a
getElemById id (a:taill) = getElemById (id - 1) taill

seed :: Int
seed = 25

getRandomElemFromList :: Int -> [a] -> a
getRandomElemFromList id list = getElemById (id `mod` (Prelude.length list)) list

generateCellsRow :: Int -> Int -> [Cell]
generateCellsRow id 0 = []
generateCellsRow id n = (getRandomElemFromList id availableCells) : (generateCellsRow n (n-1))

-- HARTCODE: source position is 0, 0
generateCellsMatrix :: Int -> Int -> [[Cell]]
generateCellsMatrix width 0 = []
-- generateCellsMatrix width 1 = (firstSource (generateCellsRow 37 width)) : (generateCellsMatrix width 0)
generateCellsMatrix width n = (generateCellsRow (width `mod` n + 37) width) : (generateCellsMatrix width (n-1))

firstSource :: [Cell] -> [Cell]
firstSource (a : t) = [source] ++ t

setAtPosition :: Integer -> a -> [a] -> [a]
setAtPosition _ obj [] = [obj]
setAtPosition 0 obj (h:taill) = obj : taill
setAtPosition n obj (h:taill) = h : (setAtPosition (n - 1) obj taill)

tailOfWorldPlayQueue :: World -> [Cell]
tailOfWorldPlayQueue (World _map (h:taill) _) = taill

headOfWorldPlayQueue :: World -> Cell
headOfWorldPlayQueue (World _map (h:taill) _) = h

setCellInWorld :: Integer -> Integer -> World -> World
setCellInWorld xId yId world
--  | (floor ((fromIntegral xId) / 173 * 100 + 1.5)) < 0 || yId < 0 = world
--  | (floor ((fromIntegral xId) / 173 * 100 + 1.5)) >= (toInteger width) || yId >= (toInteger height) = world
  | otherwise          = World {
    worldMap = setAtPosition (floor ((fromIntegral yId) / 3 * 2)) row (worldMap world) -- 0 for test
    , playQueue = (tailOfWorldPlayQueue world) ++ [getRandomElemFromList ((fromInteger (xId + yId + 1)) `mod` (Prelude.length availableCells)) availableCells]
    , debug = translated 10 8 ((lettering (pack (show (floor ((fromIntegral yId) / 173 * 100 + 1.5)))))) -- <> translated 12 8 ((lettering (pack (show ((fromIntegral yId) / 3 * 2))))) -- renderGrid (getCentersCoords (worldMap world) 1)
  }
  where
    row = setAtPosition (floor ((fromIntegral xId) / 173 * 100 + 1.5)) (headOfWorldPlayQueue world) (getElemById (floor ((fromIntegral yId) / 3 * 2)) (worldMap world)) -- 0 for test
    renderGrid :: [[(Double, Double)]] -> Picture
    renderGrid [] = blank
    renderGrid (row : t) = (renderRow row) <> (renderGrid t)

    renderRow :: [(Double, Double)] -> Picture
    renderRow [] = blank
    renderRow ((x, y) : t) = (translated x y (colored red (circle (10 * gridScale)))) <> (renderRow t)

--
--getCellIndex :: World -> Integer -> Integer -> (Maybe (Int, Int))
--getCellIndex world x y = find coords x y
--  where
--    coords = (getCentersCoords (worldMap world) 1)
--    find :: [[(Double, Double)]] -> Integer -> Integer -> (Maybe (Int, Int))
--    find [] _ _ = Nothing
--    find (row : tail) xx yy
--      | (findInRow row xx yy)
--    findIf :: (Maybe (Int, Int)) ->



--getCentersCoords :: [[Cell]] -> Double -> [[(Double, Double)]]
--getCentersCoords [] _ = []
--getCentersCoords (row : tail) i = [(getCoordsInRow row (86.6 - (mod' i 2) * 86.6) (150 * i))] ++ (getCentersCoords tail (i + 1))
--  where
--    getCoordsInRow :: [Cell] -> Double -> Double -> [(Double, Double)]
--    getCoordsInRow [] _ _         = []
--    getCoordsInRow (cell : t) x y = [((x + shiftX) * gridScale, (y + shiftY - 150) * gridScale)] ++ (getCoordsInRow t (x + 173) y)
--    shiftX :: Double
--    shiftX = ((-86.6) * fromIntegral width)
--    shiftY :: Double
--    shiftY = ((fromIntegral height) * (-50))


updatePressedCell :: World -> Maybe (Double, Double) -> World
updatePressedCell world Nothing = world
updatePressedCell world (Just (xId, yId)) = setCellInWorld (floor (xId + 7)) (floor (yId + 6)) world

getXYElem :: [[a]] -> Int -> Int -> a
getXYElem arr x y = getElemById x (getElemById y arr)

setXYElem :: [[a]] -> Int -> Int -> a -> [[a]]
setXYElem arr x y el = setAtPosition (fromIntegral y) (setAtPosition (fromIntegral x) el (getElemById y arr)) arr