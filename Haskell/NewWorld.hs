import Data.List(sort)
import System.IO
--Data Types
sumofNums = sum[1..1000]
modEx = 5 `mod` 4

--sqrt (square) returns a floating point number, same as "/", but "div" returns an Integer
--sqrt :: Floating a => a -> a 
sqrtOf = sqrt (fromIntegral 9) --converting Int to Float

--built in math functions
piVal = pi --pi
ePow9 = exp 9 --e^9
logOf9 = log 9 --log 9
squared9 = 9 ** 2 --9^2
squared8 = 8 ^ 2 --8^2
truncateVal = truncate 9.999 --9
roundVal = round 9.999 --10
ceilingVal = ceiling 9.999 --10
floorVal = floor 9.999 --9
--Also sin, cos, tan, asin, atan, acos, asinh, tanh, cosh, asinh, atanh, acosh

--Logical Operators
trueandfalse = True && False
trueortalse = True || False
notTrue = not(True)

--Guards (switch-case)
isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True
--shorter version
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

--let, in
--The let keyword allows you to declare a new variable or function within a specific scope,
--you can use it to create a new variable or function within a block of code, 
--and that variable or function will only be accessible within that block of code.
foo x y = let a = x + y
              b = x * y
          in a + b
          
--where clause
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
    | avg <= 0.200 = "Terrible Batting Average"
    | avg <= 0.250 = "Average Player"
    | avg <= 0.280 = "You're doing pretty good"
    | otherwise = "You're a Superstar"
    where avg = hits / atBats --no need to calculate in every line

--If else
doubleEvenNumber y =
    if (y `mod` 2 /= 0)
        then y
        else y * 2
--case statemets
getClass :: Int -> String
getClass n = case n of
    5 -> "Go to Kindergarten"
    6 -> "Go to elementary school"
    _ -> "Go away"

--enumerated types, list of possible types
data BaseballPlayer = Pitcher
                    | Catcher
                    | Infielder
                    | Outfield
                deriving Show                --deriving Show allows you to use it as a string
barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

barryInOF = print(barryBonds Outfield)

--custom types, store multiple values like a struct
--data Name = Name Type1, ...,
data Customer = Customer String String Double --define all the types you want inside
    deriving Show                             --deriving Show allows you to use it as a string

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main St" 20.50
getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b --_ means we don't care about the value, as we want the balance b

data RPS = Rock | Paper | Scissors --RPS is the type, Rock, Paper, Scissors are the values
shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper beats Rock"
shoot Rock Scissors = "Rock beats Scissors"
shoot Scissors Paper = "Scissors beats Paper"
shoot Scissors Rock = "Scissors loses to Rock"
shoot Paper Scissors = "Paper loses to Scissors"
shoot Rock Paper = "Rock loses to Paper"
shoot _ _ = "Error" --_ means we don't care about the value

--polymporphic types, can hold any type
--Circle is the type, Float Float Float are the values
--Rectangle is the type, Float Float Float Float are the values
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving Show
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y) --abs is absolute value
--"$" gets rid of parentheses, so you don't have to do (abs (x2 - x))

--more "$"
sumValue = putStrLn (show (1 + 2)) --putStrLn takes a string, show converts to string
sumValue2 = putStrLn . show $ 1 + 2 --same as above, but shorter

--TypeClasses
--Eq, Ord, Show, Read, Enum, Bounded, Num, Integral, Floating
--Eq is used for types that support equality testing
--Ord is for types that have an ordering
--Show members can be presented as strings
--Read strings can be presented as members
--Enum members are sequentially ordered types, they can be enumerated
--Bounded members have an upper and a lower bound
--Num is a numeric typeclass, its members have the property of being able to act like numbers
--Integral only holds whole numbers
--Floating only holds floating point numbers

data Employee = Employee { name :: String,
                            position :: String,
                            idNum :: Int
                            } deriving (Eq, Show) --deriving Eq allows you to use == or /=, deriving Show allows you to use it as a string
samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 1000}	--create a new Employee	
pamMarx = Employee {name = "Pam Marx", position = "Sales", idNum = 1001} --create a new Employee

isSamPam = samSmith == pamMarx --False
samSmithData = show samSmith --"Employee {name = \"Sam Smith\", position = \"Manager\", idNum = 1000}"

--Type Instances
data ShirtSize = S|M|L
instance Eq ShirtSize where
    S == S = True
    M == M = True
    L == L = True
    _ == _ = False

instance Show ShirtSize where
    show S = "Small"
    show M = "Medium"
    show L = "Large"

smallAvailable = S `elem` [S, M, L] --True
theSize = show S --"Small"

--custom typeclass
class MyEq a where
    areEqual :: a -> a -> Bool --a is a type variable, can be any type
data NewShirtSize = S2|M2|L2
instance MyEq NewShirtSize where
    areEqual S2 S2 = True
    areEqual M2 M2 = True
    areEqual L2 L2 = True
    areEqual _ _ = False

newSize = areEqual M2 M2 --True

--IO
sayHello = do
    putStrLn "What's your name"
    name <- getLine
    putStrLn $ "Hello " ++ name

writeToFile = do
    theFile <- openFile "test.txt" WriteMode
    hPutStrLn theFile ("Random line of text")
    hClose theFile

readFromFile = do
    theFile2 <- openFile "test.txt" ReadMode
    contents <- hGetContents theFile2
    putStr contents
    hClose theFile2

--fib sequence
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)] 
--creates a list from left to right, start with 1 and 1
--zip: lets take the fib and tail of fib and pass to the function (a,b)

--1st time: fib = 1, (tail fib) = 1, so [1,1,2]: a = 1, b = 1, so 1 + 1 = 2
--2nd time: fib = 1, (tail fib) = 2, so [1,1,2,3]: a = 1, b = 2, so 1 + 2 = 3
fib300 = fib !! 300 --gets the 300th element of the list
first20fib = take 20 fib --gets the first 20 elements of the list




 

