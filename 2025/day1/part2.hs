module Main where

import Input

password :: [(Char, Integer)] -> Integer
password insts =
  let count0s [] p acc = acc
      count0s (('L', n):is) p acc =
        count0s is ((p - n) `mod` 100)
                   (acc + (n + (100 - p) `mod` 100) `div` 100)
      count0s (('R', n):is) p acc =
        count0s is ((p + n) `mod` 100)
                   (acc + (p + n) `div` 100)
   in count0s insts 50 0

main :: IO ()
main = print $ password rotations
