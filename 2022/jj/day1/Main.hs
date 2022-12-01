module Main where

import Data.List

adventFunc :: [[Int]] -> [Int]
adventFunc xs = sort $ map (\ys -> foldl (+) 0 ys) (filter (not . null) xs)

main :: IO ()
main = interact $ show . adventFunc . map (map read) . scanl (\acc x -> if x == "" then [] else x:acc) [] . lines 
