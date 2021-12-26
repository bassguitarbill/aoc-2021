module Main where

import Text.Read
import Data.Maybe

main :: IO()
main = undefined

data Instruction = 
  Input         String        |
  AddImmediate  String Int    |
  AddRegister   String String |
  SubImmediate  String Int    |
  SubRegister   String String |
  MulImmediate  String Int    |
  MulRegister   String String |
  DivImmediate  String Int    |
  DivRegister   String String |
  ModImmediate  String Int    |
  ModRegister   String String |
  EqlImmediate  String Int    |
  EqlRegister   String String
  deriving(Show)

type Program = [Instruction]
type State = (Program, [Int], Int, Int, Int, Int)

setInput :: State -> Int -> State
setInput (p, _, w, x, y, z) inp = (p, digits inp, w, x, y, z)

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

mnRange :: [Int]
mnRange = [99999999999999, 99999999999998 ..]

isValidModelNumber :: String -> Int -> Bool
isValidModelNumber input mn = if elem 0 $ digits mn then False else z == 0
  where (_, _, _, _, _, z) = runProgram $ setInput (initialState input) mn

runProgram :: State -> State
runProgram s@([], _, _, _, _, _) = s
runProgram s = runProgram $ runInstruction s

runInstruction :: State -> State
runInstruction s@((Input _):_, _, _, _, _, _) = input s
runInstruction s@((AddImmediate _ _):_, _, _, _, _, _) = addImmediate s
runInstruction s@((AddRegister _ _):_, _, _, _, _, _) = addRegister s
runInstruction s@((SubImmediate _ _):_, _, _, _, _, _) = subImmediate s
runInstruction s@((SubRegister _ _):_, _, _, _, _, _) = subRegister s
runInstruction s@((MulImmediate _ _):_, _, _, _, _, _) = mulImmediate s
runInstruction s@((MulRegister _ _):_, _, _, _, _, _) = mulRegister s
runInstruction s@((DivImmediate _ _):_, _, _, _, _, _) = divImmediate s
runInstruction s@((DivRegister _ _):_, _, _, _, _, _) = divRegister s
runInstruction s@((ModImmediate _ _):_, _, _, _, _, _) = modImmediate s
runInstruction s@((ModRegister _ _):_, _, _, _, _, _) = modRegister s
runInstruction s@((EqlImmediate _ _):_, _, _, _, _, _) = eqlImmediate s
runInstruction s@((EqlRegister _ _):_, _, _, _, _, _) = eqlRegister s

input :: State -> State
input ((Input "w"):is, toInput:tis, _, x, y, z) = (is, tis, toInput, x, y, z)
input ((Input "x"):is, toInput:tis, w, _, y, z) = (is, tis, w, toInput, y, z)
input ((Input "y"):is, toInput:tis, w, x, _, z) = (is, tis, w, x, toInput, z)
input ((Input "z"):is, toInput:tis, w, x, y, _) = (is, tis, w, x, y, toInput)

addImmediate :: State -> State
addImmediate ((AddImmediate "w" i):is, tis, w, x, y, z) = (is, tis, w + i, x, y, z)
addImmediate ((AddImmediate "x" i):is, tis, w, x, y, z) = (is, tis, w, x + i, y, z)
addImmediate ((AddImmediate "y" i):is, tis, w, x, y, z) = (is, tis, w, x, y + i, z)
addImmediate ((AddImmediate "z" i):is, tis, w, x, y, z) = (is, tis, w, x, y, z + i)

addRegister :: State -> State
addRegister ((AddRegister "w" "w"):is, tis, w, x, y, z) = (is, tis, w + w, x, y, z)
addRegister ((AddRegister "w" "x"):is, tis, w, x, y, z) = (is, tis, w + x, x, y, z)
addRegister ((AddRegister "w" "y"):is, tis, w, x, y, z) = (is, tis, w + y, x, y, z)
addRegister ((AddRegister "w" "z"):is, tis, w, x, y, z) = (is, tis, w + z, x, y, z)

addRegister ((AddRegister "x" "w"):is, tis, w, x, y, z) = (is, tis, w, x + w, y, z)
addRegister ((AddRegister "x" "x"):is, tis, w, x, y, z) = (is, tis, w, x + x, y, z)
addRegister ((AddRegister "x" "y"):is, tis, w, x, y, z) = (is, tis, w, x + y, y, z)
addRegister ((AddRegister "x" "z"):is, tis, w, x, y, z) = (is, tis, w, x + z, y, z)

addRegister ((AddRegister "y" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y + w, z)
addRegister ((AddRegister "y" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y + x, z)
addRegister ((AddRegister "y" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y + y, z)
addRegister ((AddRegister "y" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y + z, z)

addRegister ((AddRegister "z" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y, z + w)
addRegister ((AddRegister "z" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y, z + x)
addRegister ((AddRegister "z" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y, z + y)
addRegister ((AddRegister "z" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y, z + z)

subImmediate :: State -> State
subImmediate ((SubImmediate "w" i):is, tis, w, x, y, z) = (is, tis, w - i, x, y, z)
subImmediate ((SubImmediate "x" i):is, tis, w, x, y, z) = (is, tis, w, x - i, y, z)
subImmediate ((SubImmediate "y" i):is, tis, w, x, y, z) = (is, tis, w, x, y - i, z)
subImmediate ((SubImmediate "z" i):is, tis, w, x, y, z) = (is, tis, w, x, y, z - i)

subRegister :: State -> State
subRegister ((SubRegister "w" "w"):is, tis, w, x, y, z) = (is, tis, w - w, x, y, z)
subRegister ((SubRegister "w" "x"):is, tis, w, x, y, z) = (is, tis, w - x, x, y, z)
subRegister ((SubRegister "w" "y"):is, tis, w, x, y, z) = (is, tis, w - y, x, y, z)
subRegister ((SubRegister "w" "z"):is, tis, w, x, y, z) = (is, tis, w - z, x, y, z)

subRegister ((SubRegister "x" "w"):is, tis, w, x, y, z) = (is, tis, w, x - w, y, z)
subRegister ((SubRegister "x" "x"):is, tis, w, x, y, z) = (is, tis, w, x - x, y, z)
subRegister ((SubRegister "x" "y"):is, tis, w, x, y, z) = (is, tis, w, x - y, y, z)
subRegister ((SubRegister "x" "z"):is, tis, w, x, y, z) = (is, tis, w, x - z, y, z)

subRegister ((SubRegister "y" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y - w, z)
subRegister ((SubRegister "y" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y - x, z)
subRegister ((SubRegister "y" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y - y, z)
subRegister ((SubRegister "y" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y - z, z)

subRegister ((SubRegister "z" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y, z - w)
subRegister ((SubRegister "z" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y, z - x)
subRegister ((SubRegister "z" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y, z - y)
subRegister ((SubRegister "z" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y, z - z)

mulImmediate :: State -> State
mulImmediate ((MulImmediate "w" i):is, tis, w, x, y, z) = (is, tis, w * i, x, y, z)
mulImmediate ((MulImmediate "x" i):is, tis, w, x, y, z) = (is, tis, w, x * i, y, z)
mulImmediate ((MulImmediate "y" i):is, tis, w, x, y, z) = (is, tis, w, x, y * i, z)
mulImmediate ((MulImmediate "z" i):is, tis, w, x, y, z) = (is, tis, w, x, y, z * i)

mulRegister :: State -> State
mulRegister ((MulRegister "w" "w"):is, tis, w, x, y, z) = (is, tis, w * w, x, y, z)
mulRegister ((MulRegister "w" "x"):is, tis, w, x, y, z) = (is, tis, w * x, x, y, z)
mulRegister ((MulRegister "w" "y"):is, tis, w, x, y, z) = (is, tis, w * y, x, y, z)
mulRegister ((MulRegister "w" "z"):is, tis, w, x, y, z) = (is, tis, w * z, x, y, z)

mulRegister ((MulRegister "x" "w"):is, tis, w, x, y, z) = (is, tis, w, x * w, y, z)
mulRegister ((MulRegister "x" "x"):is, tis, w, x, y, z) = (is, tis, w, x * x, y, z)
mulRegister ((MulRegister "x" "y"):is, tis, w, x, y, z) = (is, tis, w, x * y, y, z)
mulRegister ((MulRegister "x" "z"):is, tis, w, x, y, z) = (is, tis, w, x * z, y, z)

mulRegister ((MulRegister "y" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y * w, z)
mulRegister ((MulRegister "y" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y * x, z)
mulRegister ((MulRegister "y" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y * y, z)
mulRegister ((MulRegister "y" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y * z, z)

mulRegister ((MulRegister "z" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y, z * w)
mulRegister ((MulRegister "z" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y, z * x)
mulRegister ((MulRegister "z" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y, z * y)
mulRegister ((MulRegister "z" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y, z * z)

divImmediate :: State -> State
divImmediate ((DivImmediate "w" i):is, tis, w, x, y, z) = (is, tis, w `div` i, x, y, z)
divImmediate ((DivImmediate "x" i):is, tis, w, x, y, z) = (is, tis, w, x `div` i, y, z)
divImmediate ((DivImmediate "y" i):is, tis, w, x, y, z) = (is, tis, w, x, y `div` i, z)
divImmediate ((DivImmediate "z" i):is, tis, w, x, y, z) = (is, tis, w, x, y, z `div` i)

divRegister :: State -> State
divRegister ((DivRegister "w" "w"):is, tis, w, x, y, z) = (is, tis, w `div` w, x, y, z)
divRegister ((DivRegister "w" "x"):is, tis, w, x, y, z) = (is, tis, w `div` x, x, y, z)
divRegister ((DivRegister "w" "y"):is, tis, w, x, y, z) = (is, tis, w `div` y, x, y, z)
divRegister ((DivRegister "w" "z"):is, tis, w, x, y, z) = (is, tis, w `div` z, x, y, z)

divRegister ((DivRegister "x" "w"):is, tis, w, x, y, z) = (is, tis, w, x `div` w, y, z)
divRegister ((DivRegister "x" "x"):is, tis, w, x, y, z) = (is, tis, w, x `div` x, y, z)
divRegister ((DivRegister "x" "y"):is, tis, w, x, y, z) = (is, tis, w, x `div` y, y, z)
divRegister ((DivRegister "x" "z"):is, tis, w, x, y, z) = (is, tis, w, x `div` z, y, z)

divRegister ((DivRegister "y" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y `div` w, z)
divRegister ((DivRegister "y" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y `div` x, z)
divRegister ((DivRegister "y" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y `div` y, z)
divRegister ((DivRegister "y" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y `div` z, z)

divRegister ((DivRegister "z" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `div` w)
divRegister ((DivRegister "z" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `div` x)
divRegister ((DivRegister "z" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `div` y)
divRegister ((DivRegister "z" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `div` z)

modImmediate :: State -> State
modImmediate ((ModImmediate "w" i):is, tis, w, x, y, z) = (is, tis, w `mod` i, x, y, z)
modImmediate ((ModImmediate "x" i):is, tis, w, x, y, z) = (is, tis, w, x `mod` i, y, z)
modImmediate ((ModImmediate "y" i):is, tis, w, x, y, z) = (is, tis, w, x, y `mod` i, z)
modImmediate ((ModImmediate "z" i):is, tis, w, x, y, z) = (is, tis, w, x, y, z `mod` i)

modRegister :: State -> State
modRegister ((ModRegister "w" "w"):is, tis, w, x, y, z) = (is, tis, w `mod` w, x, y, z)
modRegister ((ModRegister "w" "x"):is, tis, w, x, y, z) = (is, tis, w `mod` x, x, y, z)
modRegister ((ModRegister "w" "y"):is, tis, w, x, y, z) = (is, tis, w `mod` y, x, y, z)
modRegister ((ModRegister "w" "z"):is, tis, w, x, y, z) = (is, tis, w `mod` z, x, y, z)

modRegister ((ModRegister "x" "w"):is, tis, w, x, y, z) = (is, tis, w, x `mod` w, y, z)
modRegister ((ModRegister "x" "x"):is, tis, w, x, y, z) = (is, tis, w, x `mod` x, y, z)
modRegister ((ModRegister "x" "y"):is, tis, w, x, y, z) = (is, tis, w, x `mod` y, y, z)
modRegister ((ModRegister "x" "z"):is, tis, w, x, y, z) = (is, tis, w, x `mod` z, y, z)

modRegister ((ModRegister "y" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y `mod` w, z)
modRegister ((ModRegister "y" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y `mod` x, z)
modRegister ((ModRegister "y" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y `mod` y, z)
modRegister ((ModRegister "y" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y `mod` z, z)

modRegister ((ModRegister "z" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `mod` w)
modRegister ((ModRegister "z" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `mod` x)
modRegister ((ModRegister "z" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `mod` y)
modRegister ((ModRegister "z" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `mod` z)

eqlImmediate :: State -> State
eqlImmediate ((EqlImmediate "w" i):is, tis, w, x, y, z) = (is, tis, w `eql` i, x, y, z)
eqlImmediate ((EqlImmediate "x" i):is, tis, w, x, y, z) = (is, tis, w, x `eql` i, y, z)
eqlImmediate ((EqlImmediate "y" i):is, tis, w, x, y, z) = (is, tis, w, x, y `eql` i, z)
eqlImmediate ((EqlImmediate "z" i):is, tis, w, x, y, z) = (is, tis, w, x, y, z `eql` i)

eqlRegister :: State -> State
eqlRegister ((EqlRegister "w" "w"):is, tis, w, x, y, z) = (is, tis, w `eql` w, x, y, z)
eqlRegister ((EqlRegister "w" "x"):is, tis, w, x, y, z) = (is, tis, w `eql` x, x, y, z)
eqlRegister ((EqlRegister "w" "y"):is, tis, w, x, y, z) = (is, tis, w `eql` y, x, y, z)
eqlRegister ((EqlRegister "w" "z"):is, tis, w, x, y, z) = (is, tis, w `eql` z, x, y, z)

eqlRegister ((EqlRegister "x" "w"):is, tis, w, x, y, z) = (is, tis, w, x `eql` w, y, z)
eqlRegister ((EqlRegister "x" "x"):is, tis, w, x, y, z) = (is, tis, w, x `eql` x, y, z)
eqlRegister ((EqlRegister "x" "y"):is, tis, w, x, y, z) = (is, tis, w, x `eql` y, y, z)
eqlRegister ((EqlRegister "x" "z"):is, tis, w, x, y, z) = (is, tis, w, x `eql` z, y, z)

eqlRegister ((EqlRegister "y" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y `eql` w, z)
eqlRegister ((EqlRegister "y" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y `eql` x, z)
eqlRegister ((EqlRegister "y" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y `eql` y, z)
eqlRegister ((EqlRegister "y" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y `eql` z, z)

eqlRegister ((EqlRegister "z" "w"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `eql` w)
eqlRegister ((EqlRegister "z" "x"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `eql` x)
eqlRegister ((EqlRegister "z" "y"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `eql` y)
eqlRegister ((EqlRegister "z" "z"):is, tis, w, x, y, z) = (is, tis, w, x, y, z `eql` z)

eql :: Int -> Int -> Int
eql a b = if a == b then 1 else 0


initialState :: String -> State
initialState str = (map parseInstruction $ lines str, [], 0, 0, 0, 0)

parseInstruction :: String -> Instruction
parseInstruction str =
  if ins == "inp" then Input $ head args else 
  if ins == "add" then
    if isInt $ last args then AddImmediate (head args) (fromJust $ readMaybe $ last args)
                         else AddRegister (head args) (last args)
  else if ins == "sub" then
    if isInt $ last args then SubImmediate (head args) (fromJust $ readMaybe $ last args)
                         else SubRegister (head args) (last args)
  else if ins == "mul" then
    if isInt $ last args then MulImmediate (head args) (fromJust $ readMaybe $ last args)
                         else MulRegister (head args) (last args)
  else if ins == "div" then
    if isInt $ last args then DivImmediate (head args) (fromJust $ readMaybe $ last args)
                         else DivRegister (head args) (last args)
  else if ins == "mod" then
    if isInt $ last args then ModImmediate (head args) (fromJust $ readMaybe $ last args)
                         else ModRegister (head args) (last args)
  else if ins == "eql" then
    if isInt $ last args then EqlImmediate (head args) (fromJust $ readMaybe $ last args)
                         else EqlRegister (head args) (last args)
  else error "Invalid instruction"
    where tokens = words str
          ins    = head tokens
          args   = tail tokens

isInt :: String -> Bool
isInt str = isJust readString
  where readString = readMaybe str :: Maybe Int
