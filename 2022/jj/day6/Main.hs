module Main where

testInputs = ["mjqjpqmgbljsphdztnvjfqwrcgsmlb",--sol (p1,p2): (7,19)
              "bvwbjplbgvbhsrlpgdmjqwftvncz",--sol: (5,23)
              "nppdvjthqldpwncqszvftbrmjlhg",--sol: (6,23)
              "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",--sol: (10,29)
              "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]--sol: (11,26)

type ListZipper a = ([a],[a])

zipRight :: ListZipper a -> ListZipper a
zipRight (x:xs, ys) = (xs, x:ys)

zipLeft :: ListZipper a -> ListZipper a
zipLeft (xs, y:ys) = (y:xs, ys)

-- customized for this problem
-- for part2, just set 4 to 14
zipList :: [a] -> ([a],[a])
zipList xs = (drop 4 xs, reverse $ take 4 xs)

checkDuplicates :: String -> Bool
checkDuplicates []     = False
checkDuplicates (x:xs) = (checkDuplicates xs) || (elem x xs)

-- Part 1: find first consecutive 4 char with no duplicates
-- Part 2: likewise, but 14 char (change first line)
findMarker :: ListZipper Char -> Int
findMarker (xs,ys) = if (checkDuplicates (take 4 ys)) 
                       then findMarker $ zipRight (xs,ys)
                       else length ys

adventFunc :: String -> Int
adventFunc = findMarker . zipList

main :: IO ()
main = interact $ show . adventFunc
