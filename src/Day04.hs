module Day04 where

import Debug.Trace (trace)

day04 :: IO ()
day04 = do
  contents <- readFile "data/day04-input.txt"
  let ls = lines contents
  let m = parse ls
  print $ part1 m
  print $ part2 m

type Matrix = [[Bool]]

acc :: Matrix -> (Int, Int) -> Bool
acc m (x, y) = (m !! y) !! x

size :: Matrix -> (Int, Int)
size m = (length $ head m, length m)

part1 :: Matrix -> Int
part1 = length . removables 

part2 :: Matrix -> Int  
part2 m = go m 0 where
  go m' cnt = 
    let rs = removables m'
    in  if null rs then cnt else go (updateMatrix m' rs) (cnt + length rs)

updateMatrix :: Matrix -> [(Int, Int)] -> Matrix
updateMatrix m rs = [ [acc m (x, y) && notElem (x, y) rs | x <- [0..fst (size m)-1]] | y <- [0..snd (size m)-1] ]

removables :: Matrix -> [(Int, Int)]
removables m = 
  [ (x, y) | x <- [0..fst (size m)-1], 
             y <- [0..snd (size m)-1],
             acc m (x, y) && neighbours m (x, y) < 4]
 
neighbours :: Matrix -> (Int, Int) -> Int
neighbours m (x, y) = 
  let adj = filter (\(a, b) -> validIndex m (a, b)) (adjacents (x, y))
  in  foldr (\(a, b) n -> if acc m (a, b) then n+1 else n) 0 adj

validIndex :: Matrix -> (Int, Int) -> Bool
validIndex m (x, y) = not $ x < 0 || y < 0 || x >= fst s || y >= snd s 
  where s = size m

adjacents :: (Int, Int) -> [(Int, Int)]
adjacents (x, y) = [(x+a, y+b) | a <- [-1, 0, 1], b <- [-1, 0, 1], a /= 0 || b /= 0]

parse :: [String] -> Matrix
parse = map (map (== '@')) 
