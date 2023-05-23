maximum_awesome :: Integer -> [Integer]
maximum_awesome 0 = []
maximum_awesome n = n : (maximum_awesome (n - 1))

liste l = [1, 2, 4, 5]

last_list :: [Integer] -> Integer
last_list (x : []) = x
last_list (x : xs) = last_list xs

{-sort_list [x] = x
sort_list (x:xs)
    | x > maxTail
-}
--maximum of a list
maximum' :: [Integer] -> Integer
maximum' [x] = x
maximum' (x : xs)
  -- \|maximum'(xs) > x = maximum' xs
  | x > maximum' (xs) = x
  | otherwise = maximum' xs

maximum''' :: (Ord a) => [a] -> a
maximum''' [] = error "maximum of empty list"
maximum''' [x] = x
maximum''' (x : xs) = max x (maximum''' xs)

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x -- same function as line 9  (x:[])
maximum'' (x : xs)
  | x > maxTail = x -- maxTail = maximum'' xs
  | otherwise = maxTail
  where
    maxTail = maximum'' xs

quadrat x = x * x -- Funktionsaufruf quadrat (quadrat2)

-- Fakultät
fakultaet :: (Num t, Ord t) => t -> t
fakultaet 0 = 1
fakultaet x =
  if x < 0
    then 1
    else x * (fakultaet (x - 1))

--length of list
length' :: [a] -> Int
length' [] = 0
--1+1+1+1+1+...
length' (x:xs) = 1 + length' xs

--tower of hanoi
move :: Integer -> String -> String -> String
move i from to = show (i) ++ ":" ++ from ++ "->" ++ to ++ " "

hanoi2 :: Integer -> String -> String -> String -> String
hanoi2 n start ziel hilf = 
  if (n==1) then move n start ziel 
  else hanoi2 (n-1) start hilf ziel
    ++ move n start ziel
    ++ hanoi2 (n-1) hilf ziel start

hanoi 1 start hilf ziel = [(start, ziel)]
hanoi n start hilf ziel = hanoi (n-1) start ziel hilf ++ 
  [( start, ziel)] ++ 
  hanoi (n-1) hilf start ziel


