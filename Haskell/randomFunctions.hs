addCounter :: [a] -> [(Int, a)]
addCounter [] = []
addCounter xs = zip [1..] xs

--foldl
test = foldl (/) 64 [4,2,4]
