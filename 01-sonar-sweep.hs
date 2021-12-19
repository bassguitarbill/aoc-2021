main = do
  contents <- readFile "data/01-data.txt"
  putStr $ "Part 1: " ++ part1 contents ++ "\n"
  putStr $ "Part 2: " ++ part2 contents ++ "\n"

sample :: String
sample = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"

part1 :: String -> String
part1 d = show $ increases $ getDepths d

part2 :: String -> String
part2 d = show $ increases $ windowReadings $ getDepths d

getDepths :: String -> [Int]
getDepths d = map read $ lines d

increases :: (Integral a) => [a] -> a
increases [] = 0
increases [x] = 0
increases (x:xs) = boolToNum ((head xs) > x) + increases (xs)

boolToNum :: Num p => Bool -> p
boolToNum True = 1
boolToNum False = 0

windowReadings :: (Integral a) => [a] -> [a]
windowReadings [] = []
windowReadings [_] = []
windowReadings [_, _] = []
windowReadings (x:y:z:rest) = sum [x,y,z] : windowReadings (y:z:rest)



















