import System.IO


cap :: Integer
cap = 4000000

fibonacciNumbers :: [Integer]
fibonacciNumbers = zipWith (+) (0:1:fibonacciNumbers) (0:fibonacciNumbers)

sumOfSmallEven :: Integer
sumOfSmallEven = sum $ filter even $ takeWhile (<cap) fibonacciNumbers

main :: IO ()
main = do 
  print sumOfSmallEven