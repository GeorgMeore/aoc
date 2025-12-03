module Main where

import Input

rating :: [Integer] -> Integer
rating bank =
  let update [m] c = [max m c]
      update (m:m':ms) c = (max m (m'*10 + c):update (m':ms) c)
      go [] (m:_) = m
      go (c:cs) maxs =
        go cs (update maxs c)
   in go bank (take 12 [0,0..])

main :: IO ()
main = print $ sum $ map rating batteries
