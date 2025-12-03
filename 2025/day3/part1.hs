module Main where

import Input

rating :: [Integer] -> Integer
rating bank =
  let go [] _ max2 = max2
      go (c:cs) max1 max2 =
        go cs (max max1 c) (max max2 (max1*10 + c))
   in go bank 0 0

main :: IO ()
main = print $ sum $ map rating batteries
