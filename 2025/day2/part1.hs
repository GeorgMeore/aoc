module Main where

import Input

digits :: Integer -> Integer
digits n =
  let go p l | p > n = l
             | otherwise = go (p * 10) (l + 1)
   in go 10 1

checkrange :: (Integer, Integer) -> [Integer]
checkrange (s, e) =
  let go h acc =
        let v = h * (10 ^ digits h) + h
         in if v > e
            then acc
            else go (h + 1) (v:acc)
      n = digits s
      p = 10 ^ (n `div` 2)
   in if n `mod` 2 == 0
      then let (lo, hi) = (s `mod` p, s `div` p)
            in go (if lo <= hi then hi else hi + 1) []
      else go p []

invalidcount :: [(Integer, Integer)] -> Integer
invalidcount ranges = foldl (+) 0 $ concatMap checkrange ranges

main :: IO ()
main = print $ invalidcount ranges
