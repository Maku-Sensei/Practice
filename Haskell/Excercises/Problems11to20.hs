import Mymodules (encode, encode2)
--problem 11
encodep11 :: Eq a => [a] -> [(String,Int, a)]
encodep11 [] = []
encodep11 (x:xs) = let (prefix, suffix) = span (== x) xs
                in if(length prefix > 0) then ("Multiple", length prefix + 1, x) : encodep11 suffix 
                else ("Single", 1, x) : encodep11 suffix

--better solution
data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
--same as encodeModified xs = map encodeHelper (encode xs), but in "point-free notatation"
encodeModified = map encodeHelper . encode2
    where
      encodeHelper (x,1) = Single x
      encodeHelper (x,n) = Multiple n x 