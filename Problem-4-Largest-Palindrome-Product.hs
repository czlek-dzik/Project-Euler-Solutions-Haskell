import System.IO


candidates :: [Integer]
candidates = [999*999, 999*999 - 1 .. 100*100]


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


factorize :: Integer -> [Integer]
factorize n = 
  let
    sqn = findFloorOfSqrt n
    primes = primesUpTo sqn
    check _ 1 = []
    check [] _ = []
    check (x:xs) k = 
      if k `mod` x == 0 
      then x : check (x:xs) (k `div` x) 
      else check xs k
  in check primes n


powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = 
  let ps = powerSet xs
  in [x : s | s <- ps] ++ ps


isProductOf3digits :: Integer -> Bool
isProductOf3digits n = 
  let potentialCandidates = filter (\x -> x > 99 && x < 1000) $ map product $ (powerSet.factorize) n
      good = filter (\x -> n `div` x > 99 && n `div` x < 1000) potentialCandidates
  in length good > 0


checkPalindrome :: Show a => a -> Bool
checkPalindrome a = 
  let s = show a
  in s == reverse s


main :: IO ()
main = do
  let
    palindromes = filter checkPalindrome candidates
  print $ head $ filter isProductOf3digits palindromes
