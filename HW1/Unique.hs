{--
Francisco Samuel Rios
F3004898
COP 4020
September 12, 2015
--}

module Unique where
import Data.List;

unique :: (Eq a) => [a] -> [a]

unique x = nub x;