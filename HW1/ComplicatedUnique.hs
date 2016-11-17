module Unique where
import Data.List;

unique :: (Eq a) => [a] -> [a]

unique = foldl(\num x -> if x `elem` num then num else num ++ [x]) []