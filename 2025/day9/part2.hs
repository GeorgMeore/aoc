import Input

cartesian :: [a] -> [(a, a)]
cartesian [] = []
cartesian (x:xs) = map (x,) xs ++ cartesian xs

type Point = (Int, Int)
type Line = (Point, Point)

-- Classic winding count shinanigans
inside :: [Line] -> Point -> Bool
inside outline (x, y) =
  let go [] count = count /= 0
      go (((x1, y1), (x2, y2)):ls) count
        | y1 == y2 && y == y1 && x >= min x1 x2 && x <= max x1 x2
            = True -- right on a horizontal line
        | x1 == x2 && x <= x1 && y >= min y1 y2 && y <= max y1 y2
            = x == x1 || if y == y1 || y == y2
                         then go ls (count + signum (y2 - y1))
                         else go ls (count + 2 * signum (y2 - y1))
        | otherwise = go ls count
   in go outline 0

border :: (Point, Point) -> [Point]
border ((x1, y1), (x2, y2)) =
  [(x1, y) | y <- [min y1 y2..max y1 y2]] ++
  [(x2, y) | y <- [min y1 y2..max y1 y2]] ++
  [(x, y1) | x <- [min x1 x2..max x1 x2]] ++
  [(x, y2) | x <- [min x1 x2..max x1 x2]]

-- This is very dumb brutforce, but it finished before
-- I could debug a more efficient approach based on checking
-- if rectangles' sides intersect with the outline
main :: IO ()
main =
  let outline = zip red (tail red ++ [head red])
      findmax [] acc = acc
      findmax (r@((x1, y1), (x2, y2)):rs) acc =
        let a = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)
         in if a <= acc || not (all (inside outline) (border r))
            then findmax rs acc
            else findmax rs a
   in print $ findmax (cartesian red) 0
