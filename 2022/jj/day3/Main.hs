module Main where

import Data.List
import Data.List.Split (chunksOf)

testInput = unlines ["vJrwpWtwJgWrhcsFMMfFFhFp",
                     "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
                     "PmmdzqPrVvPwwTWBwg",
                     "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
                     "ttgJtRGJQctTZtZT",
                     "CrZsJsPPZsGzwwsLwLmpwMDw"]

processInput :: String -> [String]
processInput = lines

halfStringLength :: String -> Int
halfStringLength = (\x -> div x 2) . length

-- part 1, compares halves of a single string
findMatchingCharHalves :: String -> String
findMatchingCharHalves xs = let hlen = halfStringLength xs in intersect (take hlen xs) (drop hlen xs)

-- part 2, compares 3 separate strings
findMatchingCharStrings :: [String]  -> String
findMatchingCharStrings xs = intersect (xs!!2) $ intersect (xs!!0) (xs!!1)

-- values are a=1 .. z=26, A=27 .. Z=52, and 'a'=97, 'A'=65
priorityConvert :: String -> Int
priorityConvert = (\xs -> let c = fromEnum $ xs!!0 in if c>96 then c-96 else c-38)

totalPriority :: [Int] -> Int
totalPriority = foldl (+) 0

adventFunc :: [String] -> Int
adventFunc = totalPriority . map priorityConvert . map findMatchingCharStrings . chunksOf 3

main :: IO ()
main = interact $ show . adventFunc . processInput
