{--
Francisco Samuel Rios
F3004898
COP 4020
September 12, 2015
--}

module DeleteNthElement where

deleteNthElement :: Int -> [a] -> [a]

deleteNthElement _ [] = []
deleteNthElement x y = take x y ++ drop (x+1) y