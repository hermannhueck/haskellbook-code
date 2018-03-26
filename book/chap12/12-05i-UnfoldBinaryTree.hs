-- 12-05h-UnfoldBinaryTree.hs
--
-- 12.5 Chapter Exercises, page 480
-- Finally something other than a list! page 489
--
module UnfoldBinaryTree where

-- Given the BinaryTree from last chapter, complete the following exercises.
-- Here’s that datatype again:

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)


-- 1. Write unfold for BinaryTree.

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f z = case f z of
    Nothing -> Leaf
    Just (left, val, right) -> Node (unfold f left) val (unfold f right)


-- 2. Make a tree builder.
-- Using the unfold function you’ve made for BinaryTree, write the following function:

-- recursive solution
treeBuild' :: Integer -> BinaryTree Integer
treeBuild' n = go n Leaf
    where
        go :: Integer -> BinaryTree Integer -> BinaryTree Integer
        go 0 node = node
        go n node = go (n-1) (Node node (n-1) node)

-- required solution using unfold
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x >=n then Nothing else Just(x+1, x, x+1)) 0

{-

Prelude> treeBuild 0
Leaf
Prelude> treeBuild 1
Node Leaf 0 Leaf
Prelude> treeBuild 2
Node (Node Leaf 1 Leaf)
0
     (Node Leaf 1 Leaf)
Prelude> treeBuild 3
Node (Node (Node Leaf 2 Leaf)
           1
           (Node Leaf 2 Leaf))
     0
     (Node (Node Leaf 2 Leaf)
           1
           (Node Leaf 2 Leaf))

-}        