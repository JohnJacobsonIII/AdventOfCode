factorial :: Integer -> Integer
factorial n = product [1..n]

main = interact $ show . factorial . read