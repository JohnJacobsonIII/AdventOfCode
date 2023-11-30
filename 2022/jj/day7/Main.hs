module Main where

--Part 1 sol: 95437
testInput = unlines ["$ cd /",
                     "$ ls",
                     "dir a",
                     "14848514 b.txt",
                     "8504156 c.dat",
                     "dir d",
                     "$ cd a",
                     "$ ls",
                     "dir e",
                     "29116 f",
                     "2557 g",
                     "62596 h.lst",
                     "$ cd e",
                     "$ ls",
                     "584 i",
                     "$ cd ..",
                     "$ cd ..",
                     "$ cd d",
                     "$ ls",
                     "4060174 j",
                     "8033020 d.log",
                     "5626152 d.ext",
                     "7214296 k"]

type Name = String
type Size = Int
data FileSystem = File Name Size
                | Dir Name [FileSystem] 
                deriving (Show)

sumApply f x y = x + (f y)

size :: FileSystem -> Int
size (File _ s) = s
size (Dir _ []) = 0
size (Dir _ fs) = foldl (sumApply size) 0 fs

-- Part 1, sum all Dir size <=10000
totalDirSize :: FileSystem -> Int
totalDirSize (File _ _) = 0
totalDirSize (Dir _ []) = 0
totalDirSize (Dir n fs) = let dsize = size (Dir n fs)
                          in if dsize <= 10000 then dsize + (foldl (sumApply totalDirSize) 0 fs)
                                               else foldl (sumApply totalDirSize) 0 fs

adventFunc :: String -> String
adventFunc = drop 0

main :: IO ()
main = interact $ show . adventFunc
