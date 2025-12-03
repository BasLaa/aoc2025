module Day01 where

day01 :: IO ()
day01 = do
  contents <- readFile "data/day01-input.txt"
  let ls = lines contents
  putStrLn "Result:"
  print $ run (map parse ls)

data Dir = L | R deriving (Read, Show, Eq)

parse :: String -> (Dir, Int)
parse str = 
    let (d, n) = splitAt 1 str
    in (read d, read n)

move :: (Dir, Int) -> Int -> Int
move (d,m) n | d == L = (n - m) `mod` 100
             | d == R = (n + m) `mod` 100

clicks :: (Dir, Int) -> Int -> Int
clicks (L,n) curr = (curr - 1) `div` 100 - (curr - n - 1) `div` 100 
clicks (R,n) curr = (curr + n) `div` 100 - curr `div` 100

count ::(Int, Int) -> (Dir, Int) -> (Int, Int)
count (n, cnt) st = 
  let new = move st n 
      cls = clicks st n 
  in (new, cnt+cls)

run :: [(Dir, Int)] -> Int
run xs = snd $ foldl count (50, 0) xs
