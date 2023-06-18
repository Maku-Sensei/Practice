import Mymodules (encode)
--problem 11
encodep11 :: Eq a => [a] -> [(String,Int, a)]
encodep11 [] = []
encodep11 (x:xs) = let (prefix, suffix) = span (== x) xs
                in if(length prefix > 0) then ("Multiple", length prefix + 1, x) : encodep11 suffix 
                else ("Single", 1, x) : encodep11 suffix

--better solution
data ListItem a = Single Char | Multiple Int Char
    deriving (Show)

encodeModified :: String -> [ListItem a]
--same as encodeModified xs = map encodeHelper (encode xs), but in "point-free notatation"
encodeModified = map encodeHelper . encode
    where
      encodeHelper (x,1) = Single x
      encodeHelper (x,n) = Multiple n x 

--problem 12
decodeModified :: [ListItem a] -> String
decodeModified a = 
    let
        dm [] = []
        dm (a:as) = first a ++ dm as
    in dm a
    where 
        first a = case a of
            Multiple n x ->
                let 
                    dummy :: Int -> Char -> String
                    dummy 0 _ = ""
                    dummy n x = x : (dummy (n-1)) x
                    --better solution would be replicate n x 
                in dummy n x
            Single x -> 
                pure x 
--problem 13
encodeDirect :: String -> [ListItem a]
encodeDirect as =  
    let 
        equalelements :: String -> String
        equalelements as = eqFilter as
        unequalelements :: String -> String
        unequalelements as = uneqFilter as
        countc :: String -> Int
        countc [] = 0
        countc (a:as) = 1 + (length $ filter (==a) as)
        listi :: String -> [ListItem a]
        listi as =
            if (unequalelements as) == [] then
                if countc (equalelements as) > 1 then [Multiple (countc (equalelements as)) (head as)] else [Single (head as)]
            else if (unequalelements as) /= [] then
                if countc (equalelements as) > 1 then  (Multiple (countc (equalelements as)) (head as)) : (listi (unequalelements as))
                else (Single (head as)) : (listi (unequalelements as))
            else []
    in listi as
    where 
        eqFilter [] = []
        eqFilter (a:[]) = [a]
        eqFilter (a:b:as) = if (a==b) then a : eqFilter (b:as) else [a]

        uneqFilter [] = []
        uneqFilter (a:[]) = []
        uneqFilter (a:b:as) = if (a/=b) then (b:as) else uneqFilter (b:as)
--problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = replicate 2 x ++ dupli xs
--better solution
dupli' xs = concatMap (replicate 2) xs