import System.IO


number :: Integer
number = 600851475143


primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2..n]
  where
    sieve [] = []
    sieve (x:xs) = x : filter (\y -> y `mod` x > 0) (sieve xs)


findFloorOfSqrt :: Integer -> Integer
findFloorOfSqrt k = find 0 k
  where
    find a b = 
      if b - a < 2 then a else 
        let 
          c = (a + b) `div` 2
          c' = c^2
        in if c' > k then find a c else find c b


findGreatestPrimeDiv :: [Integer] -> Integer -> Integer
findGreatestPrimeDiv p n = 
  if least == n then least else findGreatestPrimeDiv p' (n `div` least)
    where
      p' = dropWhile (\x -> n `mod` x /= 0) p
      least = head p'
  

main :: IO ()
main = do
  let primes = primesUpTo $ findFloorOfSqrt number
  print $ findGreatestPrimeDiv primes number