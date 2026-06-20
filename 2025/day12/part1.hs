import Input

count :: (a -> Bool) -> [a] -> Integer
count _ [] = 0
count p (x:xs)
  | p x = 1 + count p xs
  | otherwise = count p xs

fits :: (Integer, Integer, [Integer]) -> Bool
fits (w, h, c) =
  let sizes = [sum (map (count (== '#')) s) | s <- shapes]
      total = sum (zipWith (*) sizes c)
   in if total > w*h
        then False
      else if (div w 3)*(div h 3) >= sum c
        then True
      else
        error "Ooops" -- oooh you motherf...

main :: IO ()
main = print $ count fits regions
