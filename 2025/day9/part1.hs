import Input

cartesian :: [a] -> [(a, a)]
cartesian [] = []
cartesian (x:xs) = map (x,) xs ++ cartesian xs

main :: IO ()
main =
  let areas = [(abs (x2 - x1) + 1) * (abs (y2 - y1) + 1) | ((x1, y1), (x2, y2)) <- cartesian red]
   in print $ maximum areas
