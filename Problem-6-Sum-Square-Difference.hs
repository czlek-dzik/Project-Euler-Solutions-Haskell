import System.IO


number :: Integer
number = 100


sumFrom1ToN :: Integer -> Integer
sumFrom1ToN n = ((1 + n) * n) `div` 2


sumOfSquaresFrom1ToN :: Integer -> Integer
sumOfSquaresFrom1ToN n = (n * (n + 1) * (2*n + 1)) `div` 6


dif :: Integer -> Integer
dif n = (sumFrom1ToN n)^2 - sumOfSquaresFrom1ToN n


main :: IO ()
main = print $ dif number