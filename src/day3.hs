main :: IO ()
main = do
  inp <- readFile "inp/day3.txt"
  putStr "Day3: Part1: "
  print $ part1 inp
  putStr "Day3: Part2: "
  -- print $ part2 inpL inpR

part1 :: String -> Int
part1 = const 1


