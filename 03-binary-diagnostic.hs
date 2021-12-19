import Data.Char
import Data.List

main = do
  contents <- readFile "data/03-data.txt"
  putStr $ "Part 1: " ++ part1 contents ++ "\n"
  putStr $ "Part 2: " ++ part2 contents ++ "\n"

part1 = show . powerConsumption . parseNumbers
part2 d = "Not implemented"

sample = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

type BitCount = (Int, Int) -- (0s, 1s)

parseNumbers :: String -> [String]
parseNumbers = lines

generateBitCounts :: Int -> [BitCount]
generateBitCounts n = take n $ repeat (0, 0)

analyzeData :: [String] -> [BitCount]
analyzeData d = foldl analyzeLine (generateBitCounts l) d
  where l = length $ head d

analyzeLine :: [BitCount] -> String -> [BitCount]
analyzeLine bc line = zipWith addBitToBitCount bc line

addBitToBitCount :: BitCount -> Char -> BitCount
addBitToBitCount (z, o) '0' = (z + 1, o)
addBitToBitCount (z, o) '1' = (z, o + 1)
addBitToBitCount _ _ = error "Bad bit"

mostCommonBits :: BitCount -> Char
mostCommonBits (z, o) = if z >= o then '0' else '1'

leastCommonBits :: BitCount -> Char
leastCommonBits (z, o) = if z <= o then '0' else '1'

binaryStringToInteger :: String -> Int
binaryStringToInteger "" = 0
binaryStringToInteger s = (2 * (binaryStringToInteger $ init s)) + if '1' == last s then 1 else 0

gammaRate :: [String] -> Int
gammaRate = binaryStringToInteger . map mostCommonBits . analyzeData

epsilonRate :: [String] -> Int
epsilonRate = binaryStringToInteger . map leastCommonBits . analyzeData

powerConsumption :: [String] -> Int
powerConsumption d = gammaRate d * epsilonRate d

mostCommonFilterStep :: ([String], String) -> ([String], String)
mostCommonFilterStep (input, acc) = (filterByMostCommon input, acc ++ [getMostCommon input])

mostCommonFilter :: ([String], String) -> ([String], String)
mostCommonFilter ([], acc) = ([], acc)
mostCommonFilter ([a], acc) = ([], acc ++ a)
mostCommonFilter x = mostCommonFilter $ mostCommonFilterStep x

filterByMostCommon :: [String] -> [String]
filterByMostCommon d = map tail $ filter (\l -> head l == mostCommon) d
  where mostCommon = getMostCommon d

getMostCommon :: [String] -> Char
getMostCommon = head . map mostCommonBits . analyzeData

leastCommonFilterStep :: ([String], String) -> ([String], String)
leastCommonFilterStep (input, acc) = (filterByLeastCommon input, acc ++ [getLeastCommon input])

leastCommonFilter :: ([String], String) -> ([String], String)
leastCommonFilter ([], acc) = ([], acc)
leastCommonFilter ([a], acc) = ([], acc ++ a)
leastCommonFilter x = leastCommonFilter $ leastCommonFilterStep x

filterByLeastCommon :: [String] -> [String]
filterByLeastCommon d = map tail $ filter (\l -> head l == leastCommon) d
  where leastCommon = getLeastCommon d

getLeastCommon :: [String] -> Char
getLeastCommon = head . map leastCommonBits . analyzeData
