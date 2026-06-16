import Input

has :: Eq a => [(a, b)] -> a -> Bool
has [] _ = False
has ((k, v):kvs) x = x == k || has kvs x

get :: Eq a => [(a, b)] -> a -> b
get ((k, v):kvs) x
  | x == k = v
  | otherwise = get kvs x

set :: Eq a => [(a, b)] -> (a, b) -> [(a, b)]
set [] p = [p]
set (p@(k, v):kvs) p'@(k', v')
  | k == k' = (k, v'):kvs
  | otherwise = p:set kvs p'

paths :: [(String, [String])] -> Integer
paths g =
  let dfs m s@(name, fft, dac)
        | has m s = m
        | otherwise =
            let fft' = fft || name == "fft"
                dac' = dac || name == "dac"
                n = [(name', fft', dac') | name' <- get g name]
                m' = foldl dfs m n
             in set m' (s, sum (map (get m') n))
      minit = [(("out", True, True),   1),
               (("out", True, False),  0),
               (("out", False, True),  0),
               (("out", False, False), 0)]
      start = ("svr", False, False)
      paths = dfs minit start
   in get paths start

main :: IO ()
main = print $ paths input
