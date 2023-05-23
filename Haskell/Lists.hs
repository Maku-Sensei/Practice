import Data.List (sort)
--Lists
primeNumbers = [3,5,7,11]
morePrime = primeNumbers ++ [13, 17]

favNums = 2 : 7 : 21 : 66 : []
multList = [[3,5,7],[11,13,17]]
morePrimes2 = 2 : morePrime

lenPrime = length morePrimes2
revPrime = reverse morePrimes2
isListEmpty = null morePrimes2
secondPrime = morePrimes2 !! 1
firstPrime = head morePrimes2
lastPrime = last morePrimes2
primeinitPrime = init morePrimes2
firstxPrimes x = take x morePrimes2
dropfirstxPrimes x = drop x morePrimes2
isxinList x = x `elem` morePrimes2
maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2
newList = [2,3,5]
prodPrimes = product newList
zeroToTen = [0..10]
evenList = [2,4..20]
letterList = ['A','C'..'Z']
many2s = take 10 (repeat 2)
many3s = replicate 10 3
cycleList = take 10(cycle[1,2,3,4,5])
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]
divideBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]
sortedList :: Ord a => [a] -> [a]
sortedList x = sort x
sumOfList = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]
listBiggerThan5 = filter (>5) morePrimes2
evensUpTo20 = takeWhile (<= 20) [2,4..]
while2 = takeWhile (== 2) [1, 2, 2, 3, 2, 1]
whilex (x:xs) = takeWhile (==x) xs --takes the the first x elements
whiledrop (x:xs)= dropWhile (==x) xs --drops the first x elemets
multOfList = foldl (*) 1 [2,3,4,5] --foldl is left fold, foldr is right fold
spanl3 = span (<3) [1,2,3,1,2,3,4,5]

--List Comprehension
pow3List = [3^n | n <- [1..10]]
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

--x:y
getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
--show converts to string
getListItems (x:[]) = "Your list starts with " ++ show x 
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems(x:xs) = "The first item is " ++ show x ++ " and the rest are " ++ show xs

--As
getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

--Higher Order Functions
times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5] --map applies a function to every item in a list
multBy4 :: [Int] -> [Int]
multBy4 [] = []
--first value of the list is x and the rest is xs
multBy4 (x:xs) = times4 x : multBy4 xs --recursion

areStringEq:: String -> String -> Bool
areStringEq [] [] = True
areStringEq(x:xs) (y:ys) = 
    x == y && areStringEq xs ys
areStringEq _ _ = False -- "_ _" stands for two inputs, the same as x1, x2
--this is without the use of if and else

--Passing a function into a function
--(Int -> Int) means we pass a function that takes an Int and returns an Int
doMult :: (Int -> Int) -> Int
doMult func = func 3 --func is like x
num3Times4 = doMult times4