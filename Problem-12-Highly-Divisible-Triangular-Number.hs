import System.IO
import Data.List

triangularNumbers :: [Integer]
triangularNumbers = scanl1 (+) [1..]


findFloorOfSqrt :: Integer -> Integer
findFloorOfSqrt k = find 0 k
  where
    find a b = 
      if b - a < 2 then a else 
        let 
          c = (a + b) `div` 2
          c' = c^2
        in if c' > k then find a c else find c b


factorize :: Integer -> [Integer]
factorize n = 
  let
    sqn = findFloorOfSqrt n
    primes' = takeWhile (<= sqn) primes
    check _ 1 = []
    check [] _ = []
    check (x:xs) k = 
      if k `mod` x == 0 
      then x : check (x:xs) (k `div` x) 
      else check xs k
  in check primes' n


primes :: [Integer]
primes = 2 : filter isPrime [3,5..]


isPrime :: Integer -> Bool
isPrime n = 
  let 
    potentialDivisors = [2..findFloorOfSqrt n]
    check [] _ = True
    check (x:xs) k = 
      if k `mod` x == 0 
      then False
      else check xs k
  in check potentialDivisors n


countDivisors :: Integer -> Integer
countDivisors n = 
  let factorization = groupBy (==) $ factorize n
  in product $ fromIntegral.(+1).length <$> factorization


result = head $ dropWhile (\x -> countDivisors x < 501) triangularNumbers


main :: IO ()
main = print result