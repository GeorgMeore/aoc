import Input

toggle :: Char -> Char
toggle '.' = '#'
toggle '#' = '.'

press :: String -> [Integer] -> String
press l b =
  let go i l [] = l
      go i (c:cs) (n:ns)
        | i == n = (toggle c):(go (i+1) cs ns)
        | otherwise = c:(go (i+1) cs (n:ns))
   in go 0 l b

steps :: (String, [[Integer]], [Integer]) -> Integer
steps (end, bs, _) =
  let bfs i c v =
        let step [] n v = bfs (i+1) n v
            step (s:ss) n v =
              let neigh = [press s b | b <- bs]
                  new = [s' | s' <- neigh, not (elem s' v)]
               in if elem end new
                  then i
                  else step ss (new ++ n) (new ++ v)
         in step c [] v
      start = ['.' | _ <- end]
   in if end == start
      then 0
      else bfs 1 [start] [start]

main :: IO ()
main = print $ sum $ map steps machines
