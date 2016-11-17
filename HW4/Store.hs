--
--          Francisco Samuel Rios
--         COP 4020 November 12, 2015
--
-- This is based on ideas and the code from the first two sections of Chapter 16 "Abstract data type"

module Store
  ( Store,
    initial, -- Store a b
    value,   -- Store a b -> a -> b
    update,  -- Store a b -> a -> b -> Store a b
    merge    -- Store a b -> Store a b -> Store a b
  ) where

newtype Store a b = Store (a -> Maybe b)

initial :: Store a b
initial = Store (\_ -> Nothing)

value :: Store a b -> a -> Maybe b
value (Store sto) v = sto v

update :: Eq a => Store a b -> a -> b -> Store a b
update (Store sto) key value  
  = Store (\key' -> if key == key' then (Just value) else sto key')

store :: Store Char Integer
store = update (update (update initial 'a' 1) 'b' 2) 'c' 3

-- Implementation of "merge" which merges two stores together
-- Derived from looking at a couple mergesort definitions on various websites, 
-- and from a review of this page https://wiki.haskell.org/Maybe

-- Takes two stores as input and produces the corresponding 
-- "merged" store as output.
merge :: Store a b -> Store a b -> Store a b
merge (Store x) (Store y) = Store (\z -> case x z of 
                                           Nothing -> y z -- If there's nothing present in x, then y gets precedence 
                                           _       -> x z) -- That's it, literally that is the only case where x doesn't get precedence