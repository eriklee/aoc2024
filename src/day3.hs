import Data.Either
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  inp <- parseInput <$> readFile "inp/day3.txt"
  putStr "Day3: Part1: "
  print $ part1 inp
  -- 163931492
  putStr "Day3: Part2: "
  print $ part2 inp

parseInput :: String -> [Inst]
parseInput = rights . fromRight [] . parse (many anything) "inp/day3.txt"

part1 :: [Inst] -> Integer
part1 =  sum . fmap doMulI

part2 :: [Inst] -> Integer
part2 [] = 0
part2 ((IDont _):is) = part2 $ dropWhile (not . isDo) is
part2 (i:is) = doMulI i + (part2 is)

type Parser = Parsec Void String
data Inst = IMul Mul | IDo Do | IDont Dont deriving (Show)

isDo :: Inst -> Bool
isDo (IDo _) = True
isDo _ = False

data Garbage = G Char deriving (Show, Eq)
data Mul = Mul Integer Integer deriving (Show, Eq)
data Do = Do deriving (Show, Eq)
data Dont = Dont deriving (Show, Eq)

anything :: Parser (Either Garbage Inst)
anything = (Right <$> 
              ((IMul <$> try mul)
              <|> (IDo <$> try doP)
              <|> (IDont <$> try dont)))
            <|> (Left <$> garbage)

garbage :: Parser Garbage
garbage = G <$> (printChar <|> char '\n')

doP :: Parser Do
doP = string "do()" >> return Do

dont :: Parser Dont
dont = string "don't()" >> return Dont

mul :: Parser Mul
mul = do
  _ <- string "mul("
  l <- read <$> many digitChar 
  _ <- char ','
  r <- read <$> many digitChar 
  _ <- char ')'
  return $ Mul l r

doMulI :: Inst -> Integer
doMulI (IMul (Mul l r)) = l * r
doMulI _ = 0

testString = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
testString2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5)"
