factorial :: Int -> Int
factorial n = 
    if n < 2 then 1 else n * factorial (n-1)


pascalCell :: Int -> Int -> Int
pascalCell n r = 
    (factorial n) `div` ((factorial r) * factorial (n - r))


pascalRow :: Int -> [Int]
pascalRow n = 
    [pascalCell n r | r <- [0..n]]


pascalTriangle :: Int -> [[Int]]
pascalTriangle k = 
    [pascalRow n | n <- [0..(k - 1)]]


main :: IO ()
main = 
    interact $ unlines . map unwords . map (map show) . pascalTriangle . read