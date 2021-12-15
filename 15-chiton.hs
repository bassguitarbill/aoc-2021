import Data.List
import Data15

type Point = (Int, Int)
type Direction = String
type Path = ([Direction], Int)
type Grid = [[Int]]

split :: String -> [Int]
split [] = []
split (n:ns) = (read [n]):(split ns)

generateGrid :: String -> Grid
generateGrid = map split . lines

getRiskAtPoint :: Point -> Grid -> Int
getRiskAtPoint (x, y) g = ((g !! y) !! x)

distanceBetween :: Point -> Point -> Int
distanceBetween (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceToDestination :: Point -> Grid -> Int
distanceToDestination p g = distanceBetween p (len - 1, wid - 1)
  where len = length g
        wid = length $ head g
 






