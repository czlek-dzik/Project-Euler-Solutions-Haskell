import System.IO

cap :: Integer
cap = 1000

sumOfMultiples :: Integer
sumOfMultiples = sum $ filter (\x -> x `mod` 3 * x`mod` 5 == 0) [1..cap - 1]

main :: IO ()
main = print sumOfMultiples