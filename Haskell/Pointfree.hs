import Data.List
import Control.Monad
import Control.Monad.Fix
--sum of a list
sum' :: [Int] -> Int
sum' xs = foldr (+) 0 xs
sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

--length of a list
lengths :: [[a]] -> [Int]
lengths ls = map length ls
lengths' :: (Foldable t) => [t a] -> [Int]
lengths' = map length 

--total number of a list of list
totalnumber :: [[a]] -> Int
totalnumber ls = sum (map length ls)
totalnumber' = sum. lengths

{-
outside . inside is a composition
\x -> outside (inside x)
\x -> outside $ inside x
\x -> outside . inside $ x
      outside . inside 
"outside composed with inside"