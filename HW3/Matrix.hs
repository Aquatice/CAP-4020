{--
Francisco Samuel Rios
F3004898
COP 4020
October 3, 2015
--}

module Matrix (Matrix, fillWith, fromRule, numRows, numColumns, 
               at, mtranspose, mmap, add, mult) 
where

-- newtype is like "data", but has some efficiency advantages
newtype Matrix a = Mat ((Int,Int),(Int,Int) -> a)

fillWith   :: (Int,Int) -> a -> (Matrix a)
fromRule   :: (Int,Int) -> ((Int,Int) -> a) -> (Matrix a)
numRows    :: (Matrix a) -> Int
numColumns :: (Matrix a) -> Int
at         :: (Matrix a) -> (Int, Int) -> a
mtranspose :: (Matrix a) -> (Matrix a)
mmap       :: (a -> b) -> (Matrix a) -> (Matrix b)
add        :: Num a => (Matrix a) -> (Matrix a) -> (Matrix a)
mult       :: Num a => (Matrix a) -> (Matrix a) -> (Matrix a)

-- Without changing what is above, implement the above functions.

-- Takes a pair (m,n) and an element e and produces a m x n matrix where all elements are e
fillWith m n = Mat(m,(\_ -> n))

-- Takes a pair (m,n) and a function g (a rule), and subsequently creates an m x n matrix
-- where the (i,j)th element is g(i,j)
fromRule m n = (Mat(m, n))

-- Finds the number of rows, while not caring about the number of columns
numRows (Mat((i,_),_)) = i

-- Basically, the opposite of numRows, this one doesn't care about the number of rows,
-- only the number of columns
numColumns (Mat((_,j),_)) = j

-- Takes an m x n matrix and a pair of integers, and returns the (i,j)th element of the matrix
at (Mat(_,n)) m = (n m)

-- Takes a matrix of size m x n and turns it into a matrix of size n x m
mtranspose (Mat((m,n), x)) = Mat((n,m), xx) where xx (i,j) = x(j,i)

-- Applies the function f to the [i,jth] position of the matrix
mmap f (Mat(i, j)) = Mat(i,f.j)

-- Ran out of time to implement add, only attempt I had made was grossly incorrect, so I excluded it
-- add x Mat(m,n) Mat(i,j) =

-- Same deal with multiply as add. I know both of these could make use of foldl to fold the operations 
-- on one position of the matrix multiplication (or addition) across the entire matrix, but I simply 
-- didn't have the time.
-- mult x Mat(m,n) Mat(i,j) =  