module Main where

-- for sort
import Data.List

processInput :: String -> [String]
processInput = lines

scanSpaces :: [String] -> [[String]]
scanSpaces = scanl (\acc x -> if x == "" then [] else x:acc) [] 

clearSublists :: [[String]] -> [[String]]
clearSublists xs = zipWith (\x y  -> if y == [] then x else []) (reverse ([]:xs)) ([]:reverse xs)

removeEmpty :: [[String]] -> [[String]]
removeEmpty = filter (not . null)

sumSublists :: [Int] -> Int
sumSublists = foldl (+) 0

adventFunc :: [String] -> [Int]
adventFunc = sort . map sumSublists . map (map read) . removeEmpty . clearSublists . scanSpaces

main :: IO ()
main = interact $ show . adventFunc . processInput
