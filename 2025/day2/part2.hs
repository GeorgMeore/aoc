module Main where

import Input

digits :: Integer -> Integer
digits n =
  let go p l | p > n = l
             | otherwise = go (p * 10) (l + 1)
   in go 10 1

repeats :: Integer -> Integer -> Bool
repeats x p =
  let a = x `mod` p
      go y = y == 0 || (y `mod` p == a && go (y `div` p))
   in go (x `div` p)

isinvalid2 :: Integer -> Bool
isinvalid2 x =
  let n = digits x
      go p l | l > n `div` 2 = False
             | n `mod` l == 0 && repeats x p = True
             | otherwise = go (p * 10) (l + 1)
   in go 10 1

checkrange :: (Integer, Integer) -> [Integer]
checkrange (s, e) = filter isinvalid2 [s..e]

invalidcount :: [(Integer, Integer)] -> Integer
invalidcount ranges = sum $ concatMap checkrange ranges

main :: IO ()
main = print $ invalidcount ranges
