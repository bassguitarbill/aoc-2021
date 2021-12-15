import Data.List (group, sort, find)
import Data.Maybe
testData =
  "NNCB\n\
  \\n\
  \CH -> B\n\
  \HH -> N\n\
  \CB -> H\n\
  \NH -> C\n\
  \HB -> C\n\
  \HC -> B\n\
  \HN -> C\n\
  \NN -> C\n\
  \BH -> H\n\
  \NC -> B\n\
  \NB -> B\n\
  \BN -> B\n\
  \BB -> N\n\
  \BC -> B\n\
  \CC -> N\n\
  \CN -> C"

realData =
  "PPFCHPFNCKOKOSBVCFPP\n\
  \\n\
  \VC -> N\n\
  \SC -> H\n\
  \CK -> P\n\
  \OK -> O\n\
  \KV -> O\n\
  \HS -> B\n\
  \OH -> O\n\
  \VN -> F\n\
  \FS -> S\n\
  \ON -> B\n\
  \OS -> H\n\
  \PC -> B\n\
  \BP -> O\n\
  \OO -> N\n\
  \BF -> K\n\
  \CN -> B\n\
  \FK -> F\n\
  \NP -> K\n\
  \KK -> H\n\
  \CB -> S\n\
  \CV -> K\n\
  \VS -> F\n\
  \SF -> N\n\
  \KB -> H\n\
  \KN -> F\n\
  \CP -> V\n\
  \BO -> N\n\
  \SS -> O\n\
  \HF -> H\n\
  \NN -> F\n\
  \PP -> O\n\
  \VP -> H\n\
  \BB -> K\n\
  \VB -> N\n\
  \OF -> N\n\
  \SH -> S\n\
  \PO -> F\n\
  \OC -> S\n\
  \NS -> C\n\
  \FH -> N\n\
  \FP -> C\n\
  \SO -> P\n\
  \VK -> C\n\
  \HP -> O\n\
  \PV -> S\n\
  \HN -> K\n\
  \NB -> C\n\
  \NV -> K\n\
  \NK -> B\n\
  \FN -> C\n\
  \VV -> N\n\
  \BN -> N\n\
  \BH -> S\n\
  \FO -> V\n\
  \PK -> N\n\
  \PS -> O\n\
  \CO -> K\n\
  \NO -> K\n\
  \SV -> C\n\
  \KO -> V\n\
  \HC -> B\n\
  \BC -> N\n\
  \PB -> C\n\
  \SK -> S\n\
  \FV -> K\n\
  \HO -> O\n\
  \CF -> O\n\
  \HB -> P\n\
  \SP -> N\n\
  \VH -> P\n\
  \NC -> K\n\
  \KC -> B\n\
  \OV -> P\n\
  \BK -> F\n\
  \FB -> F\n\
  \FF -> V\n\
  \CS -> F\n\
  \CC -> H\n\
  \SB -> C\n\
  \VO -> V\n\
  \VF -> O\n\
  \KP -> N\n\
  \HV -> H\n\
  \PF -> H\n\
  \KH -> P\n\
  \KS -> S\n\
  \BS -> H\n\
  \PH -> S\n\
  \SN -> K\n\
  \HK -> P\n\
  \FC -> N\n\
  \PN -> S\n\
  \HH -> N\n\
  \OB -> P\n\
  \BV -> S\n\
  \KF -> N\n\
  \OP -> H\n\
  \NF -> V\n\
  \CH -> K\n\
  \NH -> P"

{-type Rule = (String, Char)
type Polymer = String 
type State = (Polymer, [Rule])


answer1 :: String -> [Int] 
answer1 d = sort $ map (\g -> length g) $ group $ sort finalPolymer
  where (finalPolymer, _) = iterateState 40 $ generateInitialState d

generateInitialState :: String -> State
generateInitialState d = (concat polymer, generateRules (tail rules))
  where (polymer, rules) = break (== "") $ lines d

generateRules :: [String] -> [Rule]
generateRules rules = map generateRule rules

generateRule :: String -> Rule
generateRule s = (head w, head $ last w)
  where w = words s

iterateState :: Int -> State -> State
iterateState 0 s = s
iterateState i (p, r) = iterateState (i-1) $ (applyRulesToPolymer p r, r)

applyRulesToPolymer :: Polymer -> [Rule] -> Polymer
applyRulesToPolymer "" _ = ""
applyRulesToPolymer [c] _ = [c]
applyRulesToPolymer p rules = applyRulesToPair (take 2 p) rules ++ applyRulesToPolymer (drop 1 p) rules

applyRulesToPair :: Polymer -> [Rule] -> Polymer
applyRulesToPair _ [] = error "No matching rules"
applyRulesToPair p ((pred, out):rs) = if p == pred then [(head p)] ++ [out] else applyRulesToPair p rs
-}

type Polymer = [(Pair, Int)]
type Pair = String
type Rule = (Pair, [Pair])
type State = (Polymer, [Rule])

initialPolymer = ["NN", "NC", "CB"]

stringToPolymer :: String -> Polymer
stringToPolymer "" = []
stringToPolymer [_] = []
stringToPolymer (s:t:ss) = (s:[t], 1):stringToPolymer(t:ss)

countGroups :: (Eq a) => [a] -> [(a, Int)]
countGroups = map (\g -> (head g, length g)) . group

generateInitialState :: String -> State
generateInitialState d = (stringToPolymer $ concat polymer, generateRules (tail rules))
  where (polymer, rules) = break (== "") $ lines d

generateRules :: [String] -> [Rule]
generateRules rules = map generateRule rules

generateRule :: String -> Rule
generateRule s = (head w, [(head initialPair):[toInsert], toInsert:[(last initialPair)]] )
  where
    w = words s
    initialPair = head w
    toInsert = last w !! 0

step :: Int -> State -> State
step 0 s = s
--step i (p, r) = step (i-1) $ (applyRulesToPolymer p r, r)

applyRulesToPolymer :: Polymer -> [Rule] -> Polymer
applyRulesToPolymer poly rules = map applyRuleToPolymer 

applyRuleToPolymer :: Rule -> (Polymer, Polymer) -> (Polymer, Polymer)
applyRuleToPolymer (i, o) (op, np) =
  if isJust (match, cnt)
  then (op, addPairToPolymer )
  else (op, np)
    where Maybe (match, cnt) = find (\(pr, _) -> i == pr) op
  


addPairToPolymer :: Int ->  Pair -> Polymer -> Polymer
addPairToPolymer toAdd pr poly =
  if isJust $ find (\(pairToMatch, _) -> pairToMatch == pr) poly
  then map (\(match, count) -> if match == pr then (match, count + toAdd) else (match, count)) poly
  else (pr, 1):poly