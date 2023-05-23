--map function

--map accepts a function aswell as a list of elements and returns a list of elements
--map:: (a->b) -> [a] -> [b]
--[a] = [a0,a1,a2]
--[b] = [b0,b1,b2]

module MapSquare where
    mapSquare a = map (^2) a --(^2) is the function and a is a list

