--concatMap function
--both concat function and map function together
list = concatMap (\x -> [x, x+1, x+2]) [1,2,3,4,5,6,7,8,9]