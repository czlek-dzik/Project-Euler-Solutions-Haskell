import System.IO


number :: Integer
number = 20

consecutiveLCM :: Integer -> Integer
consecutiveLCM n = foldl1 lcm [1..n]


main = print $ consecutiveLCM number