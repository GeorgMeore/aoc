module Input where

numbers :: [String]
numbers =
  ["123 328  51 64 ",
   " 45 64  387 23 ",
   "  6 98  215 314"]

operations :: [Integer -> Integer -> Integer]
operations = [(*), (+), (*), (+)]
