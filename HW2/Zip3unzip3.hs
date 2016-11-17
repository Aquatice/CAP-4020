{--
Francisco Samuel Rios
F3004898
COP 4020
September 29, 2015
--}

module Zip3unzip3 where
import Prelude hiding (zip3, unzip3)

{-- zip3 takes 3 seperate lists and compacts them into one list comprised of the elements of those lists --}
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]

{-- recursively calls itself to iterate through all 3 lists, and add their values to the new list --}
zip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3 xs ys zs 

{-- Allows us to catch an empty list with a wild-card operators --}
zip3 _ _ _ = []

unzip3 :: [(a,b,c)] -> ([a], [b], [c])

{-- Similar to above, except this folds the trio of zipped lists back into a trio of independent lists --}
unzip3 = foldr(\(x, y, z) ~(xs, ys, zs) -> (x:xs, y:ys, z:zs)) ([],[],[])