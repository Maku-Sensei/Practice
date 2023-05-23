import Data.List
--last element of a list
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--last but one element of a list
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast (x:xs:[]) = x
myButLast (x:xs) = myButLast xs

--find the k element of a list
elementAt :: Integer -> [Integer] -> Integer
elementAt k [] = error "Empty list"
elementAt 0 (x:xs) = 0
elementAt k (x:xs) =  
    if (k > 1) 
        then elementAt (k-1) xs
    else x
--length of the list
exlist = [1,2,3,5,6]
myLength = length exlist
--reverse the list=

myReverse xs = reverse xs

--is List palindrome

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = if (xs == (reverse xs)) then True else False

data NestedList a = Elem a | List [NestedList a]
instance Show a => Show (NestedList a) where
    show (Elem a) = show a
    show (List xs) = "[" ++ intercalate ", " (map show xs) ++ "]"
    
myList :: NestedList Int
myList = List [Elem 1, List [Elem 2, Elem 3], Elem 4]
flatten :: NestedList a -> [a]
flatten (Elem a)   = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten(List xs)

--eliminate duplicate Elements in a List
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x `elem` xs then compress xs else x: compress xs

--pack consecutive duplicates of list elements into sublists
pack :: Eq a =>[a] -> [[a]]
pack [] = [[]]
pack (x:y:xs) = if x == y then [x,y] : pack xs else pack (x:y:xs)

--other solution 
pack2 :: Eq a => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x : takeWhile (==x) xs) : pack2 (dropWhile (==x) xs)

--run-length encoding of a list
encode :: Eq a => [a] -> [(a,Int )]
encode [] = []
--encode (x:xs) = (length (filter (==x) xs)) + 1
encode (x:xs) = (x, length (filter (==x) xs) + 1) : encode (filter (/=x) xs)

encode2 :: Eq a => [a] -> [(Int, a)]
encode2 [] = []
encode2 (x:xs) = let (prefix, suffix) = span (== x) xs
                in (length prefix + 1, x) : encode2 suffix