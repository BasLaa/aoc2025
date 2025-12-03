module Day02 where

day02 :: IO ()
day02 = do
  contents <- readFile "data/day02-input.txt"
  let ls = head $ lines contents  
  print $ partTwo $ parse ls

type Input = [(String, String)]

partOne :: Input -> Int
partOne inp = go inp 0 where
  go [] n = n
  go ((a,b):xs) n =
    let m = foldl (\acc k -> if twice (show k) then acc + k else acc) n [(read a)..(read b)]
    in go xs m

partTwo :: Input -> Int
partTwo inp = go inp 0 where
  go [] n = n 
  go ((a,b):xs) n =
    let m = foldl (\acc k -> if splittable (show k) then acc + k else acc) n [(read a)..(read b)]
    in go xs m

twice :: Eq a => [a] -> Bool
twice xs = 
  let ln = length xs
      split = ln `div` 2
  in even ln && (take split xs == drop split xs) 

splittable :: Eq a => [a] -> Bool
splittable xs = go xs [] where
  go (y:ys) zs = splitsInto (zs ++ (y:ys)) zs || go ys (zs ++ [y])
  go _ _ = False

splitsInto :: Eq a => [a] -> [a] -> Bool
splitsInto _ [] = False
splitsInto [] _ = True
splitsInto xs ys = 
  let l = length ys
  in  (take l xs == ys) && splitsInto (drop l xs) ys

parse :: String -> Input
parse str = map (splits (== '-')) (wordsWhen (== ',') str)

splits :: (Char -> Bool) -> String -> (String, String)
splits p xs =
  let (a, b) = break p xs
  in case b of
       []      -> (a, "")
       (_:bs)  -> (a, bs)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
