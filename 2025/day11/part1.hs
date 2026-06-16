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
  let dfs m s
        | has m s = m
        | otherwise =
            let n  = (get g s)
                m' = foldl dfs m n
             in set m' (s, sum (map (get m') n))
   in get (dfs [("out", 1)] "you") "you"

main :: IO ()
main = print $ paths input
