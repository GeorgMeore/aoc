import Input

advance :: [Integer] -> String -> ([Integer], Integer)
advance beams row =
  let go new [] splits = (new, splits)
      go new ((i, 'S'):cs) splits =
        go (i:new) cs splits
      go new ((i, '.'):cs) splits
        | i `elem` beams =  go (i:new) cs splits
        | otherwise = go new cs splits
      go new ((i, '^'):cs) splits
        | i `elem` beams =  go (i-1:i+1:new) cs (splits + 1)
        | otherwise = go new cs splits
   in go [] (zip [0,1..] row) 0

run :: [String] -> Integer
run diagram =
  let go _ [] splits = splits
      go beams (row:rows) splits =
        let (beams', n) = advance beams row
         in go beams' rows (splits + n)
   in go [] diagram 0

main :: IO ()
main = print $ run diagram
