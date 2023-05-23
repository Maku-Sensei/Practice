import Mymodules (encode)
--problem 11
encode2 :: Eq a => [a] -> [(String,Int, a)]
encode2 [] = []
encode2 (x:xs) = let (prefix, suffix) = span (== x) xs
                in if(length prefix > 0) then ("Multiple", length prefix + 1, x) : encode2 suffix 
                else ("Single", 1, x) : encode2 suffix

--better solution
data ListItem a = Single a | Multiple a Int
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
--same as encodeModified xs = map encodeHelper (encode xs), but in "point-free notatation"
encodeModified = map encodeHelper . encode
    where
      encodeHelper (x,1) = Single x
      encodeHelper (x,n) = Multiple x n