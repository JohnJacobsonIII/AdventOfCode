module Main where

countIncreasing :: [Int] -> Int
countIncreasing xs = foldl (\acc x -> if x == True then acc + 1 else acc) 0 (zipWith (<) xs $ drop 1 xs)

main :: IO ()
main = interact $ show . countIncreasing . map read . concat . map words . lines
