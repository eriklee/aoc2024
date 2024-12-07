import Data.Array

-- WordSearch
type WS = Array (Int, Int) Char
type Dir = (Int, Int) -> (Int, Int)

main :: IO ()
main = do
  inp <- buildArray <$> readFile "inp/day4.txt"
  -- let inp = buildArray testString
  putStr "Day4: Part1: "
  print $ part1 inp
  putStr "Day4: Part2: "
  print $ part2 inp

part1 :: WS -> Int
part1 ws = sum . fmap (checkAllDirs ws) . findChar 'X' $ ws

part2 :: WS -> Int
part2 ws = length . filter id . fmap (isMasXd ws) . findChar 'A' $ ws

findChar :: Char -> Array (Int,Int) Char -> [(Int,Int)]
findChar c arr = filter (\i -> c == (arr ! i)) . range $ bounds arr

checkAllDirs :: WS -> (Int, Int) -> Int
checkAllDirs ws xc = length . filter id . fmap (checkDir ws xc) $ [right, left, up, down, ur, dr, ul, dl]

checkDir :: WS -> (Int, Int) -> Dir -> Bool
checkDir ws xc d = (inRange (bounds ws) end) && (isXmas ws cs)
  where 
    cs = take 4 . iterate d $ xc
    end = last cs

isXmas :: WS -> [(Int, Int)] -> Bool
isXmas = matches "XMAS"

isMasXd :: WS -> (Int, Int) -> Bool
isMasXd ws ac = (checkXDir [ul, id, dr] ws ac) && (checkXDir [dl, id, ur] ws ac)

checkXDir :: [Dir] -> WS -> (Int, Int) -> Bool
checkXDir coordF ws ac = (all (inRange $ bounds ws) coords) && (matches "MAS" ws coords || matches "SAM" ws coords)
  where coords = fmap (\f -> f ac) coordF

matches :: String -> WS -> [(Int, Int)] -> Bool
matches s ws = (s ==) . (fmap (ws !))

right, left, up, down, ur,dr,ul,dl :: Dir
right (x,y) = (x, y + 1)
left (x,y) = (x, y - 1)
up (x,y) = (x + 1, y)
down (x,y) = (x - 1, y)
ur = up . right
dr = down . right
ul = up . left
dl = down . left

testString :: String
testString = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"

buildArray :: String -> WS
buildArray inp = listArray ((0,0),(rl,cl)) joined
  where
    inpLines = lines inp
    rl = pred $ length inpLines
    cl = pred $ length (head inpLines)
    joined = concat inpLines

