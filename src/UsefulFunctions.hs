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

getRandomElemFromList :: Int -> [a] -> a
getRandomElemFromList id list = getElemById (id `mod` (Prelude.length list)) list

generateCellsRow :: Int -> Int -> [Cell]
generateCellsRow id 0 = []
generateCellsRow id n = (getRandomElemFromList id availableCells) : (generateCellsRow (generateNewSeed id) (n-1))

-- HARDCODED: source position is 0, 0
generateCellsMatrix :: Int -> Int -> Int -> [[Cell]]
generateCellsMatrix width 0 _    = []
generateCellsMatrix width n seed = (generateCellsRow seed width) : (generateCellsMatrix width (n - 1) (generateNewSeed (seed + 1)))

firstSource :: [Cell] -> [Cell]
firstSource (a : t) = [source] ++ t

setAtPosition :: Int -> a -> [a] -> [a]
setAtPosition _ obj [] = [obj]
setAtPosition 0 obj (h:taill) = obj : taill
setAtPosition n obj (h:taill) = h : (setAtPosition (n - 1) obj taill)

tailOfWorldPlayQueue :: World -> [Cell]
tailOfWorldPlayQueue (World _map (h:taill) _ _ _) = taill

headOfWorldPlayQueue :: World -> Cell
headOfWorldPlayQueue (World _map (h:taill) _ _ _) = h

setCellInWorld :: Double -> Double -> World -> World
setCellInWorld xId yId world
  | xId >= (-2) && xId <= (-1.5) && yId >= (-2) && yId <= (-1)              = World { -- restart button
    worldMap = setXYElem (setXYElem (generateCellsMatrix width height (seed world)) 0 0 source) sinkX sinkY sink
    , playQueue = availableCells
    , debug = blank
    , seed = seed world
    , menu = Menu {
      clock = blank
      , restartButton = (scaled 30 30 (lettering (pack "restart"))) <> (colored red (solidRectangle 100 100))
    }
  }
  | fst ind < 0      || snd ind < 0       = world
  | fst ind >= width || snd ind >= height = world
  | fst ind == sinkX && snd ind == sinkY  = world -- sink
  | fst ind == 0     && snd ind == 0      = world -- source cant be changed
  | otherwise          = World {
    worldMap = setAtPosition (snd ind) row (worldMap world) -- 0 for test
    , playQueue = (tailOfWorldPlayQueue world) ++ [getRandomElemFromList ((seed world) `mod` (Prelude.length availableCells)) availableCells]
    , debug = translated 1 8 ((lettering (pack (show ind)))) -- <> translated 12 8 ((lettering (pack (show ((fromIntegral yId) / 3 * 2))))) -- renderGrid (getCentersCoords (worldMap world) 1)
    , menu = (menu world)
    , seed = generateNewSeed (seed world)
  }
  where
    -- TODO row change!!!! TODO
    row = setAtPosition (fst ind) (headOfWorldPlayQueue world) (getElemById (snd ind) (worldMap world)) -- 0 for test
    renderGrid :: [[(Double, Double)]] -> Picture
    renderGrid [] = blank
    renderGrid (row : t) = (renderRow row) <> (renderGrid t)

    renderRow :: [(Double, Double)] -> Picture
    renderRow [] = blank
    renderRow ((x, y) : t) = (translated x y (colored red (circle (10 * gridScale)))) <> (renderRow t)

    ind = getCentreIndex (changeCoordsSys (xId, yId))

updatePressedCell :: World -> Maybe (Double, Double) -> World
updatePressedCell world Nothing = world
updatePressedCell world (Just (xId, yId)) = setCellInWorld (xId + 8.67) (yId + 5) world

changeCoordsSys :: (Double, Double) -> (Double, Double)
changeCoordsSys (x, y) = Prelude.foldr fun (10000, 10000) (getCentreCoords 0)
  where
    fun :: (Double, Double) -> (Double, Double) -> (Double, Double)
    fun (x1, y1) (x2, y2)
      | sqrt((x1-x)^2+(y1-y)^2) <= sqrt((x2-x)^2+(y2-y)^2) = (x1, y1)
      | otherwise = (x2, y2)
getCentreCoords :: Int -> [(Double, Double)]
getCentreCoords y
  | y == height - 1 = Prelude.take width (Prelude.map (\a -> get (a, fromIntegral(height - 1))) numbers)
  | otherwise       = (Prelude.take width (Prelude.map (\a -> get (a, fromIntegral(y))) numbers)) ++ (getCentreCoords (y + 1))
  where
    get :: (Double, Double) -> (Double, Double)
    get (xx, yy)
      | yy `mod'` 2 <= 0.1 = (xx * 1.7320508, yy * 1.5)
      | otherwise          = (0.8660254 + xx * 1.7320508, 0.5 + yy * 1.5)

getCentreIndex :: (Double, Double) -> (Int, Int)
getCentreIndex (x, y)
  | y `mod'` 1 <= 0.1 = (floor (x / 1.7320508),floor(y / 1.5))
  | otherwise         = (floor((x - 0.8660254) / 1.7320508),floor((y - 0.5) / 1.5))

numbers :: [Double]
numbers = 0 : (Prelude.map (+1) numbers)


getXYElem :: [[a]] -> Int -> Int -> a
getXYElem arr x y = getElemById x (getElemById y arr)

setXYElem :: [[a]] -> Int -> Int -> a -> [[a]]
setXYElem arr x y el = setAtPosition (fromIntegral y) (setAtPosition (fromIntegral x) el (getElemById y arr)) arr

generateNewSeed :: Int -> Int
generateNewSeed a = (a * 51 + 13) `mod` 1000000007


isNoFlood :: [[Cell]] -> Bool
isNoFlood [] = True
isNoFlood (hh : tt) = (isNoLineFlood hh) && (isNoFlood tt)

isNoLineFlood :: [Cell] -> Bool
isNoLineFlood [] = True
isNoLineFlood (hh : tt)
  | (isWatered hh) == Flooded = False
  | otherwise                 = isNoLineFlood tt