module Main where

import Data.List (sortBy)
import Data.List.Split (splitOn)

processInput :: String -> [[String]]
processInput = map lines . splitOn "\n\n"

sumSublists :: [Int] -> Int
sumSublists = foldl (+) 0

adventFunc :: [[String]] -> [Int]
adventFunc = take 3 . sortBy (flip compare) . map sumSublists . map (map read)

main :: IO ()
main = interact $ show . adventFunc . processInput
