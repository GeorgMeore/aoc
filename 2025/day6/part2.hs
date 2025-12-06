module Main where

import Input
import Debug.Trace

isws :: Char -> Bool
isws c = c == ' ' || c == '\t' || c == '\n'

isdigit :: Char -> Bool
isdigit c = c >= '0' && c <= '9'

digit :: Char -> Integer
digit '0' = 0
digit '1' = 1
digit '2' = 2
digit '3' = 3
digit '4' = 4
digit '5' = 5
digit '6' = 6
digit '7' = 7
digit '8' = 8
digit '9' = 9

str2int :: String -> Integer
str2int s =
  let s' = dropWhile isws (reverse s)
      d  = takeWhile isdigit s'
   in sum [digit c * 10^p | (c, p) <- zip d [0,1..]]

chop1col :: [String] -> ([Integer], [String])
chop1col rows =
  let go col rows =
        if all null rows || all (== ' ') (map head rows)
        then (col, map tail rows)
        else go (str2int (map head rows):col) (map tail rows)
   in go [] rows

calc :: [String] -> [(Integer -> Integer -> Integer)] -> [Integer]
calc _ [] = []
calc nums (op:ops) =
  let (col, nums') = chop1col nums
   in foldl1 op col:calc nums' ops

main :: IO ()
main = print $ sum $ calc numbers operations
