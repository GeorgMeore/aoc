module Main where

import Input

sort :: (a -> a -> Bool) -> [a] -> [a]
sort _ [] = []
sort le (x:xs) = sort le (filter (not . le x) xs) ++ x:sort le (filter (le x) xs)

countFresh :: [(Integer, Integer)] -> Integer
countFresh fresh =
  let count [(s, e)] = e - s + 1
      count ((s1, e1):(s2, e2):rest)
        | e1 < s2 = (e1 - s1 + 1) + count ((s2, e2):rest)
        | otherwise = count ((s1, max e1 e2):rest)
   in count (sort (\(a, _) (b, _) -> a < b) fresh)

main :: IO ()
main = print $ countFresh fresh
