import System.IO


number :: Integer
number = 10001


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


findFloorOfSqrt :: Integer -> Integer
findFloorOfSqrt k = find 0 k
  where
    find a b = 
      if b - a < 2 then a else 
        let 
          c = (a + b) `div` 2
          c' = c^2
        in if c' > k then find a c else find c b


result :: Integer
result = primes !! fromIntegral (number - 1)


main :: IO ()
main =
    print result