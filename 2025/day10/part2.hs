import Input

pop1 :: (a -> Bool) -> [a] -> (a, [a])
pop1 p (x:xs)
  | p x = (x, xs)
  | otherwise = let (v, t) = pop1 p xs
                 in (v, x:t)

gauss :: [[Integer]] -> [[Integer]]
gauss [] = []
gauss [eq] = [eq]
gauss ([]:_) = []
gauss eqs
  | all (== 0) (map head eqs) =
      let (h:t) = eqs
       in h:gauss (map tail t)
  | otherwise =
      let (h@(x:xs), t) = pop1 ((/= 0) . head) eqs
       in h:gauss [[x*a - h'*b | (a, b) <- zip t' xs] | (h':t') <- t]

steps :: (String, [[Integer]], [Integer]) -> Integer
steps (_, bs, end) =
  let eqs = [[if elem e b then 1 else 0 | b <- bs] ++ [j]
             | (e, j) <- zip [0..] end]
      eqs' = take (length bs) (gauss eqs)
      (r, c) = (length eqs', length bs)
      m = maximum end -- max amount of times any button can be pressed
      search [] =
        let guesses 0 = [[-1]]
            guesses n = [a:g | a <- [0..m], g <- guesses (n-1)]
         in guesses (c - r)
      search ((h:t):eqs)
        | h == 0 =
            [a:s | s <- search eqs,
                   let v  = sum [a*b | (a, b) <- zip t s],
                   v == 0,
                   a <- [0..m]]
        | otherwise =
            [div v h:s | s <- search eqs,
                         let v  = -sum [a*b | (a, b) <- zip t s],
                         v*h >= 0 && mod v h == 0]
      presses = search eqs'
   in 1 + minimum (map sum presses)

main :: IO ()
main = print $ sum $ map steps machines
