import Data.Array
import Data.Maybe
import Data.Monoid (Sum, getSum)
import qualified Data.Set as S

type Lab = Array (Int, Int) Char
type Dir = (Int, Int) -> (Int, Int)

data Facing = N | E | S | W deriving (Show, Eq, Ord)
data Guard = Guard Facing (Int, Int) deriving (Show, Eq, Ord)

main :: IO ()
main = do
  inp <- buildArray <$> readFile "inp/day6.txt"
  -- inp <- buildArray <$> readFile "inp/day6_test.txt"
  putStr "Day6: Part1: "
  print $ part1 inp
  putStr "Day6: Part2: "
  print $ part2 inp

part1 :: Lab -> Int
part1 lab = getSum . countVisited . fromExit $ patrolGuard step guard lab'
  where (guard, lab') = mkGuard lab

part2 :: Lab -> Int
part2 lab = length . filter isLoop $ patrolGuard step2 guard <$> obsLabs
  where (guard, lab') = mkGuard lab
        obsLabs = addObstacles lab'

countVisited :: Lab -> (Sum Int)
countVisited = foldMap (\elem -> if elem == 'X' then 1 else 0)

patrolGuard :: Step -> Guard -> Lab -> StopCond
patrolGuard stepf guard lab = runToLeft stepf $ St lab guard mempty

data StopCond = Loop | Exit Lab deriving (Eq, Show)
fromExit :: StopCond -> Lab
fromExit (Exit l) = l
fromExit _ = error "StopCond was not Exit!" -- sorry, not sorry

isLoop :: StopCond -> Bool
isLoop Loop = True
isLoop _ = False

data State = St { lab:: Lab, guard :: Guard, guardHist :: S.Set Guard }
type Step = State -> Either StopCond State

step :: State -> Either StopCond State
step St{guard = g, lab = l, guardHist = hist} = 
  let
    cycle = S.member g hist
    hist' = S.insert g hist
    path = guardPath g
    (visit, rest) = span (\c -> inRange (bounds l) c && ('#' /= (l ! c))) path
    lab' = markVisited l visit
  in 
    case (cycle, (inRange (bounds l) $ head rest)) of
      (True, _) -> Left Loop
      (False, False) -> Left (Exit lab')
      (False, True) -> Right $ St lab' (Guard (rot $ gFacing g) (last visit)) hist'

step2 :: State -> Either StopCond State
step2 St{guard = g, lab = l, guardHist = hist} = 
  let
    cycle = S.member g hist
    hist' = S.insert g hist
    path = guardPath g
    (visit, rest) = span (\c -> inRange (bounds l) c && ('#' /= (l ! c))) path
  in 
    case (cycle, (inRange (bounds l) $ head rest)) of
      (True, _) -> Left Loop
      (False, False) -> Left (Exit l)
      (False, True) -> Right $ St l (Guard (rot $ gFacing g) (last visit)) hist'

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

addObstacles :: Lab -> [Lab]
addObstacles l = catMaybes $ addObstacle l <$> (indices l)

addObstacle :: Lab -> (Int, Int) -> Maybe Lab
addObstacle l c | l ! c == '.' = Just $ l // [(c, '#')]
addObstacle _ _  = Nothing

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
  let (_ht,wd) = snd $ bounds l
  let contents = chunk (wd + 1) $ elems l
  mapM_ putStrLn contents

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l):(chunk n $ drop n l)
