testData = [3,4,3,1,2] :: [Int]

realData :: [Int]
realData = [3,5,1,2,5,4,1,5,1,2,5,5,1,3,1,5,1,3,2,1,5,1,1,1,2,3,1,3,1,2,1,1,5,1,5,4,5,5,3,3,1,5,1,1,5,5,1,3,5,5,3,2,2,4,1,5,3,4,2,5,4,1,2,2,5,1,1,2,4,4,1,3,1,3,1,1,2,2,1,1,5,1,1,4,4,5,5,1,2,1,4,1,1,4,4,3,4,2,2,3,3,2,1,3,3,2,1,1,1,2,1,4,2,2,1,5,5,3,4,5,5,2,5,2,2,5,3,3,1,2,4,2,1,5,1,1,2,3,5,5,1,1,5,5,1,4,5,3,5,2,3,2,4,3,1,4,2,5,1,3,2,1,1,3,4,2,1,1,1,1,2,1,4,3,1,3,1,2,4,1,2,4,3,2,3,5,5,3,3,1,2,3,4,5,2,4,5,1,1,1,4,5,3,5,3,5,1,1,5,1,5,3,1,2,3,4,1,1,4,1,2,4,1,5,4,1,5,4,2,1,5,2,1,3,5,5,4,5,5,1,1,4,1,2,3,5,3,3,1,1,1,4,3,1,1,4,1,5,3,5,1,4,2,5,1,1,4,4,4,2,5,1,2,5,2,1,3,1,5,1,2,1,1,5,2,4,2,1,3,5,5,4,1,1,1,5,5,2,1,1]

data FishAge = FishAge Int Int Int Int Int Int Int Int Int deriving(Show) -- holy shit

createFishAgeElements :: [Int] -> [Int]
createFishAgeElements [] = [0,0,0,0,0,0,0,0,0]
createFishAgeElements (a:as) = (fst arrayHalves) ++ [((nextGen !! a) + 1)] ++ (tail $ snd arrayHalves)
  where arrayHalves = splitAt a nextGen
        nextGen = createFishAgeElements as

createFishAge :: [Int] -> FishAge
createFishAge (f0:f1:f2:f3:f4:f5:f6:f7:f8:[]) = FishAge f0 f1 f2 f3 f4 f5 f6 f7 f8
createFishAge _ = FishAge 0 0 0 0 0 0 0 0 0 

generate :: FishAge -> FishAge
generate (FishAge f0 f1 f2 f3 f4 f5 f6 f7 f8) = FishAge f1 f2 f3 f4 f5 f6 (f7 + f0) f8 f0 

generateTimes :: Int -> FishAge -> FishAge
generateTimes 0 fa = fa
generateTimes n fa = generateTimes (n - 1) (generate fa)

fishPopulation :: FishAge -> Int 
fishPopulation (FishAge f0 f1 f2 f3 f4 f5 f6 f7 f8) = sum [f0, f1, f2, f3, f4, f5, f6, f7, f8]