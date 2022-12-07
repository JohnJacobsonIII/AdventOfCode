module Main where

-- for sort
--import Data.List
--import Data.List.Split (splitOn)
-- import Text.Parsec
import Data.Either (fromRight)
import Data.List.Split (splitOn, chunksOf)
import Data.List (transpose)

testInput = unlines ["    [D]    ",
                     "[N] [C]    ",
                     "[Z] [M] [P]",
                     " 1   2   3 ",
                     "",
                     "move 1 from 2 to 1",
                     "move 3 from 1 to 3",
                     "move 2 from 2 to 1",
                     "move 1 from 1 to 2"]

data Instruction = Instruction { move :: Int
                               , from :: Int
                               , to   :: Int
                               } deriving (Show)


splitSections :: String -> [String]
splitSections = splitOn "\n\n"

parseStacks :: String -> [[Char]]
parseStacks = map (filter (\x -> x /= ' ')) . 
              transpose . 
              map (map (\xs -> xs!!1)) .
              map (chunksOf 4) . 
              init . lines

extractValues = (\xs -> let [_, move, _, from, _, to] = xs
                        in Instruction (read move)
                                       ((read from)-1) -- 1 indexed
                                       ((read to)-1) )

parseInstructions :: String -> [Instruction]
parseInstructions = map extractValues . map words . lines

-- extract value from monad and read to Int's
-- couldn't get parsing to Int's working directly
-- processInput :: String -> [[String]] -> 
-- processInput = (\x -> zipWith ($) (parseStacks, parseInstructions) (splitSections x))

mutateStacks :: Int -> [Char] -> [[Char]] -> [[Char]]
mutateStacks idx newStack stacks = let (xs, ys) = splitAt idx stacks
                                   in xs++(newStack:(drop 1 ys))

removeFromStack :: [[Char]] -> Instruction -> [Char]
removeFromStack s i = let fromStack = s!!(from i)
                      in drop (move i) fromStack

-- part 1
addToStack :: [[Char]] -> Instruction -> [Char]
addToStack s i = let toStack = s!!(to i)
                     fromStack = s!!(from i)
                 in (reverse (take (move i) fromStack)) ++ toStack 

-- part 2
addToStackMulti :: [[Char]] -> Instruction -> [Char]
addToStackMulti s i = let toStack = s!!(to i)
                          fromStack = s!!(from i)
                      in (take (move i) fromStack) ++ toStack 

moveStacks :: [[Char]] -> Instruction -> [[Char]]
moveStacks stacks instr = mutateStacks (from instr)
                                       (removeFromStack stacks instr) 
                                       (mutateStacks (to instr) 
                                                    (addToStack stacks instr)
                                                    (stacks))

adventFunc :: String -> [Char]
adventFunc = (\x -> let [stacks, instructions] = splitSections x in 
              map head $ foldl moveStacks
                               (parseStacks stacks)
                               (parseInstructions instructions))

main :: IO ()
main = interact $ show . adventFunc




-- 
-- 
-- splitSections :: String -> [String]
-- splitSections = splitOn "\n\n"
-- 
-- stackParser :: Parsec String st [[String]]
-- stackParser = 
-- 
-- instructionParser :: Parsec String st [Instruction]
-- instructionParser = (string "move ") *> cellParser *>
--                       (string " from ") *> cellParser *>
--                       (string " to ") cellParser
--                     where cellParser = many1 digit
-- 
-- -- parsing to inefficient list structure for now
-- -- each range is a list of two string numbers (interval endpoints), 
-- -- and each pair is a list of two strings (two ranges)
-- csvParser :: Parsec String st [[[String]]]
-- csvParser = endBy lineParser newline <* eof
--               where lineParser = sepBy rangeP (char ',')
--                     rangeP = sepBy cellParser (char '-')
--                     cellParser = many $ noneOf "-,\n"
