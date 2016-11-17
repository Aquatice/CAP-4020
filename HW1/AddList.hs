{--
Francisco Samuel Rios
F3004898
COP 4020
September 12, 2015
--}

module AddList where

-- Beginning of AddListComprehension

add_list_comprehension :: Integer -> [Integer] -> [Integer]

add_list_comprehension x y = [x + n | n <- y]

-- End of AddListComprehension

-- Beginning of AddListRecursion

add_list_recursion :: Integer -> [Integer] -> [Integer]

add_list_recursion x [] = []
add_list_recursion x (y:ys) = y + x : add_list_recursion x ys

-- End of AddListRecursion

-- Beginning of AddListMap

add_list_map :: Integer -> [Integer] -> [Integer]

-- Function that handles the addition to be mapped
addNum :: Integer -> Integer -> Integer
addNum n i = n + i 

add_list_map x y = map (addNum x) y

-- End of AddListMap