module Day03 where
import Data.Char (digitToInt)
import Data.List (elemIndex, intercalate)

day03 :: IO ()
day03 = do
  contents <- readFile "data/day03-input.txt"
  let ls = lines contents
  print $ part1 ls
  print $ part2 ls

part1 :: [String] -> Int
part1 ls = sum $ map (find . toInts) ls

part2 :: [String] -> Int
part2 ls = sum $ map (find2 . toInts) ls

toInts :: String -> [Int]
toInts = map digitToInt

find :: [Int] -> Int
find xs = 
  let n = maximum (init xs)
      m = maximum $ rest xs n 
  in read (show n ++ show m)

find2 :: [Int] -> Int
find2 xs = read $ concatMap show (go xs 12 [])
  where 
    go _ 0 zs = zs 
    go ys n zs = 
      let m = maximum (dropLast (n - 1) ys)  
          rs = rest ys m
      in  go rs (n-1) (zs ++ [m])

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs

rest :: [Int] -> Int -> [Int]
rest xs n = case elemIndex n xs of
  Nothing -> error "oh no"
  Just i -> drop (i+1) xs

