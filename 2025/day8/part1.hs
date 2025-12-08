import Input

type Point = (Integer, Integer, Integer)

sort :: (a -> a -> Bool) -> [a] -> [a]
sort _ [] = []
sort le (x:xs) = sort le (filter (not . le x) xs) ++ x:sort le (filter (le x) xs)

dist :: Point -> Point -> Integer
dist (x1, y1, z1) (x2, y2, z2) = (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2

dists :: [Point] -> [(Integer, (Integer, Integer))]
dists points =
  let go [] = []
      go ((i, p):ps) = [(dist p p', (i, j)) | (j, p') <- ps] ++ go ps
   in go (zip [0,1..] points)

connect :: Eq a => [[a]] -> (a, a) -> [[a]]
connect sets (x, y) =
  let go [] [] [] = [[x, y]]
      go [] [] [yset] = [x:yset]
      go [] [xset] [] = [y:xset]
      go [] hasx hasy = [(concat hasx) ++ (concat hasy)]
      go (s:sets) hasx hasy
        | x `elem` s && y `elem` s = s:sets
        | x `elem` s = go sets (s:hasx) hasy
        | y `elem` s = go sets hasx (s:hasy)
        | otherwise = s:go sets hasx hasy
   in go sets [] []

main :: IO ()
main =
  let d = sort (<) (dists boxes)
   in print $ product $ take 3 $ sort (>) $ map length $ foldl connect [] $ map snd $ take 1000 d
