module Main where

shapes :: String -> Int
shapes "A" = 1
shapes "B" = 2
shapes "C" = 3
shapes "X" = 1
shapes "Y" = 2
shapes "Z" = 3

processInput :: String -> [[String]]
processInput = map words . lines

shapeScores :: [[String]] -> [[Int]]
shapeScores = map (map shapes)

-- if X,Y,Z are shapes, W/L is determined by difference. Each shape beats previous shape
scoreFuncShape :: [Int] -> Int
scoreFuncShape xs = (3 * (mod ((xs!!1 - xs!!0) + 1) 3)) + xs!!1

-- if X,Y,Z are W/L (part 2), determine points by subtracting from first value
scoreFuncWL :: [Int] -> Int
scoreFuncWL xs = (3 * (xs!!1 - 1)) + (mod (xs!!0 + xs!!1) 3) + 1

totalScore :: [Int] -> Int
totalScore = foldl (+) 0

adventFunc :: [[String]] -> Int
adventFunc = totalScore . map scoreFuncWL . shapeScores

main :: IO ()
main = interact $ show . adventFunc . processInput
