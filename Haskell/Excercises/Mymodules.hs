module Mymodules (encode) where 

    data Encoded a = Multiple Char Int | Single Char deriving (Eq, Show)

    encode :: String -> [(Char,Int )]
    encode [] = []
    encode (x:xs) = let (prefix, suffix) = span (==x) xs
                    in (x, length (filter (==x) prefix) + 1) : encode suffix

{-
    encode2 :: Eq a => [a] -> [Encoded a]
    encode2 [] = []
    encode2 [x] = [Single x]
    encode2 (x:xs) = case encode2 xs of
        (Single y : rest) -> if x == y then (Multiple x 2) : rest else (Single x) : (Single y) : rest
        (Multiple y n : rest) -> if x == y then (Multiple x (n + 1)) : rest else (Single x) : (Multiple y n) : rest
-}