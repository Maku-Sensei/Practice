--Permutation is defined as "each of several possible ways in which a set or number of things can be ordered or arranged.

--concatMap creates a list from a list generating function by application of this function on all elements in a list
--passed as the second argument
import Data.List (permutations)
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (interleave x) (permutations xs)
    where
        interleave x [] = [[x]]
        --x is inserted into the list y:ys, then via map
        interleave x (y:ys) = (x:y:ys): map (y:) (interleave x ys) 

--for strings, but needs to import Data.Lists
stringPermutations :: String -> [String]
stringPermutations str = map (\chars -> foldr (:) [] chars) (permutations str)

