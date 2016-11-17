{--
Francisco Samuel Rios
F3004898
COP 4020
September 29, 2015
--}

module Tree where

data Tree a = Nil | Node a (Tree a) (Tree a) 
                deriving (Eq, Show)

depth :: Tree a -> Integer
depth Nil            = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

-- collapses a tree into a list by visiting 
-- the elements of the tree 'inorder'

collapse :: Tree a -> [a]
collapse Nil            = []
collapse (Node x t1 t2) = collapse t1 ++ [x] ++ collapse t2

-- stratifies a tree into a list by visiting
-- all elements at depth 1, then all elements 2, etc.

-- So the program basically asked for a breadth-first traversal of the tree, so I did a breadth first traversal of the tree!
stratify :: Tree a -> [a]
stratify tree = breadthFirst [tree]
    where
        -- Case for an empty list/tree
        breadthFirst [] = []

        -- If nothing can be gathered here, move on
        breadthFirst (Nil : xs) = breadthFirst xs

        -- Holds on to the root node, left node, and right node at the current location
        -- Appends the root, and then recursively goes to the left and right nodes (left first)
        -- Then the left and right nodes will be the roots for their recursive calls which repeat 
        -- the process given above!
        breadthFirst (Node root left right : xs) = root : breadthFirst (xs ++ [left, right])