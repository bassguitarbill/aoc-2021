main = do
  contents <- readFile "data/02-data.txt"
  putStr $ "Part 1: " ++ part1 contents ++ "\n"
  putStr $ "Part 2: " ++ part2 contents ++ "\n"

part1 d = show $ horizontalTimesVertical  $ applyCommands  (parseCommands d) (0, 0)
part2 d = show $ horizontalTimesVertical2 $ applyCommands2 (parseCommands d) (0, 0, 0)

sample :: String
sample = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

data Command = Up Int | Down Int | Forward Int deriving(Show)
type Position = (Int, Int)
type Position2 = (Int, Int, Int)

parseCommands :: String -> [Command]
parseCommands d = map parseCommand $ lines d

parseCommand :: String -> Command
parseCommand ('f':'o':'r':'w':'a':'r':'d':' ':rs) = Forward $ read rs
parseCommand ('d':'o':'w':'n':' ':rs) = Down $ read rs
parseCommand ('u':'p':' ':rs) = Up $ read rs

applyCommands :: [Command] -> Position -> Position
applyCommands [] p = p
applyCommands (c:cs) p = applyCommands cs $ applyCommand c p

applyCommand :: Command -> Position -> Position
applyCommand (Forward amt) = (\(x, y) -> (x + amt, y))
applyCommand (Up amt) = (\(x, y) -> (x, y - amt))
applyCommand (Down amt) = (\(x, y) -> (x, y + amt))

applyCommands2 :: [Command] -> Position2 -> Position2
applyCommands2 cs p = foldl applyCommand2 p cs

applyCommand2 :: Position2 -> Command -> Position2
applyCommand2 p (Forward amt) = (\(x, y, aim) -> (x + amt, y + (aim * amt), aim)) p
applyCommand2 p (Up amt) = (\(x, y, aim) -> (x, y, aim - amt)) p
applyCommand2 p (Down amt) = (\(x, y, aim) -> (x, y, aim + amt)) p

horizontalTimesVertical :: Position -> Int
horizontalTimesVertical = (\(x, y) -> x * y)

horizontalTimesVertical2 :: Position2 -> Int
horizontalTimesVertical2 = (\(x, y, _) -> x * y)
