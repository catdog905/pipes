module UsefulFunctions where

import Graphics.Gloss
import Objects
import System.Random

-- | First element in four elements tuple. 
firstOfTuple :: (a,b,c,d) -> a
firstOfTuple (x, _, _, _) = x

-- | Second element in four elements tuple. 
secondOfTuple :: (a,b,c,d) -> b
secondOfTuple (_, x, _, _) = x

-- | Third element in four elements tuple. 
thirdOfTuple :: (a,b,c,d) -> c
thirdOfTuple (_, _, x, _) = x

-- | Fourth element in four elements tuple. 
fourthOfTuple :: (a,b,c,d) -> d
fourthOfTuple (_, _, _, x) = x

-- | Vector by 2 points (p1 to p2)
vectorDiff :: Point -> Point -> Vector
vectorDiff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- | Vector length
vectorMagnitude :: Vector -> Float
vectorMagnitude (x, y) = sqrt (x**2 + y**2)

-- | Distance between 2 points
distance :: Point -> Point -> Float
distance points = vectorMagnitude.vectorDiff points

-- | Multiply vector by scalar
vectorMul :: Vector -> Float -> Vector
vectorMul (x, y) a = (x * a, y * a)

-- | Adjust vector to some length
normalizeVector :: Vector -> Float -> Vector
normalizeVector (x, y) l = (x * k, y * k)
  where
    k = l / vectorMagnitude (x, y)

-- | Sum of vectors
vectorSum :: [Vector] -> Vector
vectorSum = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0,0)






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
