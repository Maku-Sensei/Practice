import Data.List
import Numeric (showIntAtBase, readInt)
import Data.Char (intToDigit, digitToInt)
import Prelude hiding (Word)
-- Word is also in prelude, hence it is hided
import Data.Char
-- imported functions from Data.Char: toLower
import System.Environment
-- imported functions from System.Environment: getArgs
import Data.List 
-- imported functions from Data.List: sort, nub
import Data.Map hiding (map,foldr,filter,null, foldl)

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

--ternary to decimal
ternaryToDecimal :: String -> Int
ternaryToDecimal ternaryNum = foldl (\acc digit -> acc * 3 + digitToInt digit) 0 ternaryNum

--modulo ternary
modt :: String -> Int
modt n = (ternaryToDecimal n) `mod` 5

--lamba (x:xs)
list = [1,2,3,4,5,6,7,8,9,10]
listeq = [1,2]
lambdalist = map (\x -> filter (/=x) list) 



type Word = String
type Sentence = [Word]
type CharCount = [(Char, Int)]
-- Ex.(Word) -> "love"
-- Ex.(Sentence) -> ["I", "love", "you"]
-- Ex.(CharCount) -> [('e', 1), ('l', 1), ('o', 1), ('v', 1)]

type WordDictionary = [(Word, CharCount)]
-- Ex.(WordDictionary) -> [("love", [('e', 1), ('l', 1), ('o', 1), ('v', 1)])]

type CharCountDictionary = [(CharCount, Sentence)]
-- Ex.(CharCountDictionary) -> [([('a', 1), ('e', 1), ('t', 1)], ["eat", "tea"])]

wordCharCounts :: Word -> CharCount
wordCharCounts cs = zip chars (repetition chars)
    where
        chars = sort $ nub lower --chars = "eghot"
        lower  = map toLower cs
        --"/= checks for unqeual elements not equal"
        repetition  = map (\c -> length (filter (==c) lower)) 
lower cs  = map toLower cs

concatenatedList cc = concat [replicate (max n 1) c | (c, n) <- cc]

ccToWord :: CharCount -> Word 
ccToWord cc = concat [replicate (max n 1) c | (c,n) <- cc]
      
--powerset :: String -> [String]
powerset [] = [[]]
powerset (c:cs) = [c:cs' | cs' <- powerset cs] ++ powerset cs

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

powersettest (c:cs) = cs

charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets = map wordCharCounts . nub . powerset  . ccToWord

atest [] = [[]]
atest (c:cs) = [c:cs' | cs' <- atest cs] ++ atest cs

powerset' :: Eq a => [a] -> [[a]]
powerset' (a:s) = if a `elem` s then ps else ps ++ map (a:) ps
                    where ps = powerset' s
powerset' _ = [[]]
