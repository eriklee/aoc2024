import Data.Array
import Data.Monoid (Sum, getSum)

type Lab = Array (Int, Int) Char
type Dir = (Int, Int) -> (Int, Int)

data Facing = N | E | S | W deriving (Show, Eq)
data Guard = Guard Facing (Int, Int) deriving (Show, Eq)

main :: IO ()
main = do
  inp <- buildArray <$> readFile "inp/day6.txt"
  -- let inp = buildArray testString
  putStr "Day6: Part1: "
  print $ part1 inp
  putStr "Day6: Part2: "
  -- print $ part2 inp

part1 :: Lab -> Int
part1 lab = getSum . countVisited $ patrolGuard lab' guard
  where (guard, lab') = mkGuard lab

countVisited :: Lab -> (Sum Int)
countVisited = foldMap (\elem -> if elem == 'X' then 1 else 0)

patrolGuard :: Lab -> Guard -> Lab
patrolGuard lab guard = runToLeft step (lab, guard)

step :: (Lab, Guard) -> Either Lab (Lab, Guard)
step (l, g) = let
  path = guardPath g
  (visit, rest) = span (\c -> inRange (bounds l) c && ('#' /= (l ! c))) path
  lab' = markVisited l visit
  in 
  if (inRange (bounds l) $ head rest) 
    then (Right (lab', Guard (rot $ gFacing g) (last visit))) 
    else (Left lab')

-- gives an infinite ray for the guard in the specified direction
-- note that N/E are sort of backwards from expectations...
guardPath :: Guard -> [(Int, Int)]
guardPath (Guard N coord) = iterate down coord
guardPath (Guard E coord) = iterate right coord
guardPath (Guard S coord) = iterate up coord
guardPath (Guard W coord) = iterate left coord

runToLeft :: (b -> Either a b) -> b -> a
runToLeft f x = case f x of
  Left a -> a
  Right x' -> runToLeft f x'

-- find the guard in the initial map, assumes facing = N
mkGuard :: Lab ->  (Guard, Lab)
mkGuard lab = (Guard N pos, lab')
  where pos = fst . head . filter (('^' ==) . snd) $ assocs lab
        lab' = markVisited lab [pos]

markVisited :: Lab -> [(Int, Int)] -> Lab
markVisited l cs = l // (zip cs $ repeat 'X')

gFacing :: Guard -> Facing
gFacing (Guard f _) = f

---- Array Nonsense
right, left, up, down :: Dir
right (x,y) = (x, y + 1)
left (x,y) = (x, y - 1)
up (x,y) = (x + 1, y)
down (x,y) = (x - 1, y)

rot :: Facing -> Facing
rot N = E
rot E = S
rot S = W
rot W = N

buildArray :: String -> Lab
buildArray inp = listArray ((0,0),(rl,cl)) joined
  where
    inpLines = lines inp
    rl = pred $ length inpLines
    cl = pred $ length (head inpLines)
    joined = concat inpLines

showArray :: Lab -> IO ()
showArray l = do
  let (ht,wd) = snd $ bounds l
  let contents = chunk (wd + 1) $ elems l
  mapM_ putStrLn contents

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l):(chunk n $ drop n l)
