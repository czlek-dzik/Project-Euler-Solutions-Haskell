import System.IO


triples :: [[Integer]]
triples = [[a,b,c] | a <- [1..333], c <- [334..500], b <- [a..c], a+b+c == 1000]


isPythagorean :: [Integer] -> Bool
isPythagorean [a,b,c] = a^2 + b^2 == c^2

result :: Integer
result = (product.concat) $ filter isPythagorean triples


main :: IO ()
main = print result