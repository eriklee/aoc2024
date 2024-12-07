import qualified Data.Map as M
import Data.Void
import Data.Maybe

import Text.Megaparsec
import Text.Megaparsec.Char

data Rule = Rule Int Int
  deriving (Show, Eq)
type Page = Int
data Update = Upd Int (M.Map Page Int)
  deriving (Show, Eq)

main :: IO ()
main = do
  Right inp <- parse input "day5.txt" <$> readFile "inp/day5.txt"
  putStr "Day5: Part1: "
  -- 
  print $ part1 inp
  putStr "Day5: Part2: "
  print $ part2 inp

part1 :: ([Rule], [Update]) -> Int
part1 (rules, updates) = sum $ (\(Upd mid ps) -> if (all (sat ps) rules) then mid else 0) <$> updates

part2 :: ([Rule], [Update]) -> Int
part2 (rules, updates) = sum $ findMid rules <$> unsat
  where unsat :: [Update] = filter (\(Upd _ ps) -> (not $ all (sat ps) rules)) updates

-- Parsing
type Parser = Parsec Void String

rule :: Parser Rule
rule = read <$> many digitChar >>= (\b ->
      char '|' >> read <$> many digitChar >>= return . Rule b)

pages :: Parser [Page]
pages = (read <$> many digitChar) `sepEndBy1` (char ',')

input :: Parser ([Rule], [Update])
input = do
  rules <- (rule <* newline) `manyTill` newline
  updates <- pages `endBy1` newline
  return (rules, mkUpdate <$> updates)

mkUpdate :: [Page] -> Update
mkUpdate ps = Upd mid pmap
  where 
    pmap = M.fromList $ flip zip [1..] ps
    mid = head . drop (length ps `div` 2) $ ps

-- part 1
sat :: M.Map Page Int -> Rule -> Bool
sat u (Rule f s) = fromMaybe True $ do
  fi <- M.lookup f u
  si <- M.lookup s u
  return (fi < si)

-- Part 2
ruleMatches :: [Page] -> Rule -> Bool
ruleMatches ps (Rule l r) = any (== l) ps && any (== r) ps

-- returns number of pages before/after this page according to rules.
-- The midpage will obviously have these numbers be equal
pagePlacement :: Page -> [Rule] -> (Int, Int)
pagePlacement p rules = foldl process (0,0) rules
  where
    process (b,a) (Rule l _) | l == p = (b+1, a)
    process (b,a) (Rule _ r) | r == p = (b, a+1)
    process acc    _ = acc

isMidPage :: [Rule] -> Page -> Bool
isMidPage rules page = b == a
  where (b,a) = pagePlacement page rules

findMid :: [Rule] -> Update -> Page
findMid allRules (Upd _ pageMap) = head . filter (isMidPage rules) $ upages
  where rules = filter (ruleMatches upages) allRules
        upages = M.keys pageMap
