-- Starting code for Assignment 3
-- CISC 260, Winter 2016
-- Tyler Dolye (10129777)
module Assignment3 where

-- Algebraic type for a binary search tree of integers.  A tree may be empty, or it 
-- may have a root, a left subtree and a right subtree.  (Note that one or both of the
-- subtrees might be empty.)  

data Tree = Empty | MakeTree Tree Int Tree

-- Creates a multi-line sideways representation of a tree.  
-- The root will be on the left, with its left child below it 
-- and its right child above it.  If you tilt your head to the
-- left you'll see the structure of the tree.
-- The second parameter is indentation level.
treeToString :: Tree -> Int -> String
treeToString Empty _ = ""
treeToString (MakeTree left root right) indent =
    rightString ++ 
    (spaceString indent) ++ (show root) ++ "\n" ++ 
    leftString
    where
    leftString = treeToString left (indent+4)
    rightString = treeToString right (indent+4)
    
-- Creates a string consisting of n spaces (assuming n >= 0)
spaceString :: Int -> String
spaceString 0 = ""
spaceString n = ' ':(spaceString (n-1))

-- treeToString will be used to display trees
instance Show Tree where show tree = treeToString tree 0

-- The sample tree given with the assignment
sampleTree = 
    (MakeTree -- tree with root 45
        (MakeTree -- tree with root 15
            (MakeTree -- tree with root 4
                (MakeTree Empty (-1) Empty)
                4
                (MakeTree Empty 7 Empty)
            )
            15
            (MakeTree Empty 23 Empty)
        )
        45
        (MakeTree -- tree with root 72
            Empty
            72
            (MakeTree -- tree with root 103
                (MakeTree Empty 99 Empty)
                103
                Empty
            )
        )
    )

-- Adds a number to a tree, producing a new tree.  No duplicates allowed,
-- so if the number already exists in the tree it is returned without
-- change.
add :: Int -> Tree -> Tree
add x Empty = MakeTree Empty x Empty
add x (MakeTree left root right)
    | x == root = MakeTree left root right -- no duplicates
    | x < root = MakeTree (add x left) root right
    | otherwise = MakeTree left root (add x right)
  
-- Assignment3 code below by Tyler Doyle (10129777)  
  
-- returns true if the tree is empty, false otherwise  
isEmpty :: Tree -> Bool
isEmpty Empty = True
isEmpty _ = False
   
-- returns the height of the tree (longest path from root to leaf)
height :: Tree -> Int
height Empty = 0
height (MakeTree left _ right) = 1 + (max (height left) (height right))

-- returns true if the value is in the tree, false otherwise
search :: Int -> Tree -> Bool
search _ Empty = False
search val (MakeTree left root right) 
 | root == val = True
 | otherwise = (search val left) || (search val right)
 
-- returns the maximum value of the tree
maxValue :: Tree -> Int
maxValue Empty = error "no max value in empty tree"
maxValue (MakeTree _ root Empty) = root
maxValue (MakeTree _ _ right) = maxValue right

-- creates a tree from a list of ints
createTree :: [Int] -> Tree -- need to use a fold
createTree [] = Empty
createTree lis = foldr (add) Empty (reverse lis) -- add ensures no duplicates

-- creates a balanced tree (minimum height) from a sorted list of ints 
balancedTree :: [Int] -> Tree
balancedTree [] = Empty
balancedTree lis = (createTree (balancedHelper (splitMiddle lis)))

--Takes 2 lists of Ints and returns a list that, when made into a tree, will be balanced 
balancedHelper :: ([Int],[Int]) -> [Int]
balancedHelper ([],[]) = []
balancedHelper (lis, (x:xs)) = x:(balancedHelper (splitMiddle lis)) ++ (balancedHelper (splitMiddle xs))

--splits a list in half
splitMiddle :: [Int] -> ([Int],[Int])
splitMiddle [] = ([],[])
splitMiddle lis = splitAt ((length lis) `div` 2) lis

-- creates a sorted list of ints from a tree
treeToList :: Tree -> [Int] 
treeToList Empty = []
treeToList (MakeTree left root right) = treeToListAccum (treeToList left) root right

--cons's a list of ints onto a list
treeToListAccum :: [Int] -> Int -> Tree -> [Int]
treeToListAccum [] root right = root:(treeToList right)
treeToListAccum [x] root right = x:root:(treeToList right)
treeToListAccum (x:xs) root right = x:(treeToListAccum xs root right)

-- deletes a node from a tree and replaces it with the left tree's max value
delete :: Int -> Tree -> Tree
delete _ Empty = Empty
delete val (MakeTree left root right)
 | root == val = deleteHelper (MakeTree left root right)
 | root > val = (MakeTree (delete val left) root right)
 | otherwise = (MakeTree left root (delete val right))
 
-- once the node is found, this helper function decides what to put in its place
deleteHelper :: Tree -> Tree
deleteHelper (MakeTree left root right)
 | isEmpty left && isEmpty right = Empty
 | isEmpty left = right
 | otherwise = (MakeTree (delete (maxValue left) left) (maxValue left) right)