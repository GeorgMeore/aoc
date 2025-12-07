import Input

type Beam = (Integer, Integer)

push :: [Beam] -> Beam -> [Beam]
push [] b = [b]
push ((i, c):bs) (i', c')
  | i == i' = (i, c+c'):bs
  | otherwise = (i', c'):(i, c):bs

advance :: [Beam] -> String -> [Beam]
advance beams row =
  let go new [] = new
      go new ((i, 'S'):cs) =
        go ((i, 1):new) cs
      go new ((i, '.'):cs) =
        case lookup i beams of
          Nothing -> go new cs
          Just c -> go (push new (i, c)) cs
      go new ((i, '^'):cs) =
        case lookup i beams of
          Nothing -> go new cs
          Just c -> go (push (push new (i-1, c)) (i+1,c)) cs
   in go [] (zip [0,1..] row)

run :: [String] -> Integer
run diagram =
  sum $ map snd $ foldl advance [] diagram

main :: IO ()
main = print $ run diagram
