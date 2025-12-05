module Main where

import Input

countFresh :: [(Integer, Integer)] -> [Integer] -> Int
countFresh fresh available =
  length $ filter (\id -> any (\(s, e) -> id >= s && id <= e) fresh) available

main :: IO ()
main = print $ countFresh fresh available
