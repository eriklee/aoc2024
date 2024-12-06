import Data.List (sort)
import qualified Data.Map as M

main :: IO ()
main = do
  inp <- lines <$> readFile "inp/day1.txt"
  let (inpL,inpR) = both sort . unzip $ parseLine <$>  inp
  putStr "Day1: Part1: "
  print $ part1 inpL inpR
  -- 3574690
  putStr "Day1: Part2: "
  print $ part2 inpL inpR

parseLine :: String -> (Int, Int)
parseLine = tuplize . take 2 . fmap read .  words 

tuplize :: [Int] -> (Int,Int)
tuplize [l,r] = (l,r)
tuplize _ = error "Bad Input"

part1 :: [Int] -> [Int] -> Int
part1 l r = sum $ abs <$> zipWith (-) l r

buildMap :: [Int] -> M.Map Int Int
buildMap = M.fromListWith (+) . fmap (,1)

calcSimilarityScore :: M.Map Int Int -> Int -> Int
calcSimilarityScore m x = maybe 0 (*x) (M.lookup x m)

part2 :: [Int] -> [Int] -> Int
part2 l r = sum $ fmap (calcSimilarityScore $ buildMap r) l

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)
