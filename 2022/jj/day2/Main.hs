module Main where

import Data.List (sortBy)
import Data.List.Split (splitOn)

outcomes :: String -> Int
-- loss
outcomes "A Z" = 0
outcomes "B X" = 0
outcomes "C Y" = 0
--draw
outcomes "A X" = 3
outcomes "B Y" = 3
outcomes "C Z" = 3
--win
outcomes "A Y" = 6
outcomes "B Z" = 6
outcomes "C X" = 6 

shapes :: Char -> Int
shapes 'X' = 1
shapes 'Y' = 2
shapes 'Z' = 3



processInput :: String -> [String]
processInput = lines

outcomeScores :: [String] -> [Int]
outcomeScores = map outcomes

shapeScores :: [String] -> [Int]
shapeScores = map shapes . map (!! 2)

-- scoreRound :: [String] -> Int
-- scoreRound = outcomes 

makeScores :: [String] -> [Int]
makeScores xs = zipWith (+) (outcomeScores xs) (shapeScores xs)

totalScore :: [Int] -> Int
totalScore = foldl (+) 0

adventFunc :: [String] -> Int
adventFunc = totalScore . makeScores

main :: IO ()
main = interact $ show . adventFunc . processInput
