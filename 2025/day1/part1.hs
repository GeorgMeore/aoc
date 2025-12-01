module Main where

import Input

password :: [(Char, Integer)] -> Integer
password insts =
  let count0s [] _ acc = acc
      count0s ((d, n):is) pos acc =
        let pos' = if d == 'L'
                   then (pos - n) `mod` 100
                   else (pos + n) `mod` 100
         in if pos' == 0
            then count0s is pos' (acc + 1)
            else count0s is pos' acc
   in count0s insts 50 0

main :: IO ()
main = print $ password rotations
