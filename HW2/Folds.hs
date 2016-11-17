{--
Francisco Samuel Rios
F3004898
COP 4020
September 29, 2015
--}

module Folds where

{-- Left fold because to find all occurences of a given value we need to traverse the entire list, 
    and left fold is the best at this --}
count :: Eq a => a -> [a] -> Integer

-- I don't think this needs explaining, but it compares the given value to the current value in the list, and increments count if they match
count x xs = foldl (\count a -> if a == x then (count + 1) else count) 0 xs

{-- Right fold because elem returns true or false depending on whether or not the specified value is 
    present anywhere in the list. In most cases, this will not need to look at the whole list, making foldr better here --}
elem2 :: Eq a => a -> [a] -> Bool

-- Pretty similar to above, only with a right fold this time. If we find a match, set the accumulator to true,
-- and if no match is found, just default to the accumulator (essentially a logical OR statement)
elem2 x xs = foldr (\y boolAcc -> if y == x then True else boolAcc) False xs
