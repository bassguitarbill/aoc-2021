smolData = 
  "11111\n\
  \19991\n\
  \19191\n\
  \19991\n\
  \11111"

testData =
  "5483143223\n\
  \2745854711\n\
  \5264556173\n\
  \6141336146\n\
  \6357385478\n\
  \4167524645\n\
  \2176841721\n\
  \6882881134\n\
  \4846848554\n\
  \5283751526"

realData =
  "2566885432\n\
  \3857414357\n\
  \6761543247\n\
  \5477332114\n\
  \3731585385\n\
  \1716783173\n\
  \1277321612\n\
  \3371176148\n\
  \1162578285\n\
  \6144726367"

type Squid = (Int, Bool, Point)
type Squidlet = (Int, Bool, Int)
type GameState = ([[Squid]], Int)
type Point = (Int, Int)

initialGameState :: String -> GameState
initialGameState d = (gridOfSquids(map (\row -> lineOfSquidlets (map (\c -> read [c]) row) (length row)) (lines d)) (length (lines d)) , 0)

gridOfSquids :: [[Squidlet]] -> Int -> [[Squid]]
gridOfSquids [] _ = []
gridOfSquids (r:rs) numRows = (map (\(energy, _, x) -> (energy, False, (x, numRows - (length rs) - 1))) r):(gridOfSquids rs numRows)

lineOfSquidlets :: [Int] -> Int -> [Squidlet]
lineOfSquidlets [] _ = []
lineOfSquidlets (n:ns) rowLength = (n, False, rowLength - (length ns) - 1):(lineOfSquidlets ns rowLength)

leftToFlash :: GameState -> [Squid] 
leftToFlash (grid, _) = filter (\(energy, flashedYet, _) -> (energy >= 9) && (flashedYet == False)) $ concat grid

getFromPoint :: GameState -> Point -> Squid
getFromPoint (grid, _) (x, y) = grid !! y !! x

step :: GameState -> GameState
step gs = executeFlashes (increaseEnergyAllSquid gs)

executeFlashes :: GameState -> GameState
executeFlashes gs = if null (leftToFlash gs) then (resetHasFlashedAllSquid gs) else executeFlashes $ flash gs

flash :: GameState -> GameState
flash gs = flashSquids (leftToFlash gs) gs

flashSquids :: [Squid] -> GameState -> GameState
flashSquids [] gs = gs
flashSquids (s:ss) gs = flashSquid s (flashSquids ss gs)

flashSquid :: Squid -> GameState -> GameState
flashSquid (_, flashedYet, (x, y)) (grid, numFlashes) =
  if flashedYet then (grid, numFlashes) else setSquidFlashedGame (x, y) (flash9 (x, y) (grid, numFlashes + 1))

flash9 :: Point -> GameState -> GameState
flash9 (x, y) (grid, numFlashes) = (take (y - 1) grid ++ map (flash3 x) (take 3 $ drop (y - 1) grid) ++ drop (y + 2) grid, numFlashes)

flash3 :: Int -> [Squid] -> [Squid]
flash3 x row = take (x - 1) row ++ map getFlashed (take 3 $ drop (x - 1) row) ++ drop (x + 2) row

getFlashed :: Squid -> Squid
getFlashed (energy, hasFlashed, pt) = (energy + 1, hasFlashed, pt)

setSquidFlashedGame :: Point -> GameState -> GameState
setSquidFlashedGame (x,y) (grid, numFlashes) = (take y grid ++ [setSquidFlashedRow x (head $ drop y grid)] ++ drop (y + 1) grid, numFlashes)

setSquidFlashedRow :: Int -> [Squid] -> [Squid]
setSquidFlashedRow x row = take x row ++ [setSquidFlashed (row !! x)] ++ drop (x + 1) row

setSquidFlashed :: Squid -> Squid
setSquidFlashed (energy, _, pt) = (energy, True, pt)

increaseEnergySquid :: Squid -> Squid
increaseEnergySquid (energy, hasFlashed, pt) = (energy + 1, hasFlashed, pt)

increaseEnergyAllSquid :: GameState -> GameState
increaseEnergyAllSquid gs = transformAllSquid increaseEnergySquid gs

transformAllSquid :: (Squid -> Squid) -> GameState -> GameState
transformAllSquid transform (grid, numFlashes) = (map (\r -> map (\s -> transform s) r) grid, numFlashes)

resetHasFlashedAllSquid :: GameState -> GameState
resetHasFlashedAllSquid gs = transformAllSquid (resetHasFlashed . resetHasTooMuchEnergySquid) gs

resetHasFlashed :: Squid -> Squid
resetHasFlashed (energy, _, pt) = (energy, False, pt)

resetHasTooMuchEnergySquid :: Squid -> Squid
resetHasTooMuchEnergySquid (energy, hasFlashed, pt) = (if hasFlashed then 0 else energy, False, pt)

dropThree :: Int -> [Int] -> [Int]
dropThree i row = take (i-1) row ++ take 3 [0, 0 ..] ++ drop (i+2) row