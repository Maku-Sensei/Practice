import Data.List

sortList :: String -> String
sortList xs = sort xs

rlist xs = xs !!0
rlist2 = \xs -> xs!!0

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- Question 2 solution, but it creates permutations 
longestAnagrams :: [String] -> String
longestAnagrams [] = []
longestAnagrams xs =   let moreAnagrams = concatMap permutations xs 
                           groupedAnagrams = groupBy (\x y -> sort x == sort y) xs
                           longest = maximumBy (\x y -> compare (length x) (length y)) (map (\xs -> xs!!0) groupedAnagrams)
                         in longest

main = do
    let myList = "cat"
    print (Data.List.permutations myList)
