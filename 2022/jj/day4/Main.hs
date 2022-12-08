module Main where

-- for sort
--import Data.List
--import Data.List.Split (splitOn)
import Text.Parsec
import Data.Either (fromRight)

testInput = unlines ["2-4,6-8",
                     "2-3,4-5",
                     "5-7,7-9",
                     "2-8,3-7",
                     "6-6,4-6",
                     "2-6,4-8"]

-- parsing to inefficient list structure for now
-- each range is a list of two string numbers (interval endpoints), 
-- and each pair is a list of two strings (two ranges)
csvParser :: Parsec String st [[[String]]]
csvParser = endBy lineParser newline <* eof
              where lineParser = sepBy rangeP (char ',')
                    rangeP = sepBy cellParser (char '-')
                    cellParser = many $ noneOf "-,\n"

-- extract value from monad and read to Int's
-- couldn't get parsing to Int's working directly
processInput :: String -> [[[Int]]]
processInput = map (map (map read)) . fromRight [] . parse csvParser ""

-- Part 1
-- for intervals [I1, I2], I :: [Int, Int],
-- check I1 subset I2 || I2 subset I1
-- by checking both endpoints of an interval
-- for membership in the opposite interval
checkIntervalSubset :: [[Int]] -> Bool
checkIntervalSubset xs = let ll = ((xs!!0)!!0)
                             lr = ((xs!!0)!!1)
                             rl = ((xs!!1)!!0)
                             rr = ((xs!!1)!!1)
                         in (ll >= rl && lr <= rr) || (ll <= rl && lr >= rr)

-- Part 2
-- for intervals [I1, I2], I :: [Int, Int],
-- check all 4 endpoints for membership in the opposite interval
-- lr = (l)eft interval, (r) endpoint
checkIntervalIntersect :: [[Int]] -> Bool
checkIntervalIntersect xs = let ll = ((xs!!0)!!0)
                                lr = ((xs!!0)!!1)
                                rl = ((xs!!1)!!0)
                                rr = ((xs!!1)!!1)
                         in (ll >= rl && ll <= rr) -- left end  of left interval in right interval
                             || (lr >= rl && lr <= rr) 
                             || (rl >= ll && rl <= lr)
                             || (rr >= ll && rr <= lr)

adventFunc :: [[[Int]]] -> Int
adventFunc = foldl (+) 0 . map (\x -> if x then 1 else 0) . map checkIntervalIntersect

main :: IO ()
main = interact $ show . adventFunc . processInput
