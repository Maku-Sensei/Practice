--functions
--do chains a bunch of commands and saves them in main
main = do
    putStrLn "What's your name"
    name <- getLine
    putStrLn ("Hello " ++ name)

--funcName param1 param2 ... = operations (returned value)
addme :: Int -> Int -> Int
addme x y = x + y
addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x,y) (x2,y2) = (x+x2, y+y2)
whatAge :: Int -> String
whatAge 16 = "You can drive"

--recursive functions
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)
--3 * factorial(2)
--2 * factorial(1)
--1 * factorial(0) = 1
--0 = 1
--then go backwards
--2 * 1 = 2
--3 * 2 = 6
prodFact n = product [1..n] -- easy way to do factorial

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

--return a function
getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y
adds3 = getAddFunc 3
fourPlus3 = adds3 4
threePlusList = map adds3 [1,2,3,4,5]

--Lambdas are anonymous functions (factions with no names)
--backslash defines a lambda, x is what is is going to be receiving, x * 2 is what it is going to return
dbl1To10 = map (\x -> x * 2) [1..10] 
