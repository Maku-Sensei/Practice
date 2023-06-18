module Mymodules (encode, encode2) where 
    data Encoded a = Multiple a Int | Single a deriving (Eq, Show)
    encode :: Eq a => [a] -> [(a,Int )]
    encode [] = []
    --encode (x:xs) = (length (filter (==x) xs)) + 1
    encode (x:xs) = (x, length (filter (==x) xs) + 1) : encode (filter (/=x) xs)

    encode2 :: Eq a => [a] -> [Encoded a]
    encode2 [] = []
    encode2 [x] = [Single x]
    encode2 (x:xs) = case encode2 xs of
        (Single y : rest) -> if x == y then (Multiple x 2) : rest else (Single x) : (Single y) : rest
        (Multiple y n : rest) -> if x == y then (Multiple x (n + 1)) : rest else (Single x) : (Multiple y n) : rest
    
