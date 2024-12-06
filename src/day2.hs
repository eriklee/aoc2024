main :: IO ()
main = do
  inp <-  lines <$> readFile "inp/day2.txt"
  let reports = map (map read . words) inp
  putStr "Day2: Part1: "
  print $ length . filter id $ part1 <$> reports
  putStr "Day2: Part2: "
  print $ length . filter id $ isSafe2 <$> reports

type Level = Int
type Report = [Level]

data LevelDiffs = LD [Int]

part1 :: Report -> Bool
part1 = isSafe . levelSteps

isSafe :: LevelDiffs -> Bool
isSafe (LD diffs) = (all (inRange (1,3)) diffs) || (all (inRange (-3,-1)) diffs)

levelSteps :: Report -> LevelDiffs
levelSteps r = LD $ zipWith (-) r (tail r)

inRange :: Ord a => (a, a) -> a -> Bool
inRange (lb, ub) x = x >= lb && x <= ub

allCombos :: Report -> [Report]
allCombos (x:xs) = (xs):(fmap (x:) $ allCombos xs)
allCombos _ = []

isSafe2 :: Report -> Bool
isSafe2 r = any isSafe steps
  where steps = levelSteps <$> r:(allCombos r)
