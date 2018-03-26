-- 11-17-BinaryTree.hs
--
-- 11.17 Binary Tree, page 446
-- Write functions for BinaryTree, page 446
--
module BinaryTree where

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
  

-- Inserting into trees

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node(insert' b left) a right
  | b > a = Node left a (insert' b right)

t1 = insert' 0 Leaf
t2 = insert' 3 t1
t3 = insert' 5 t2


-- Write map for BinaryTree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay :: IO()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup! mapTree okay!"
  else error "test failed!"


-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = inorder left ++ [x] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = postorder left ++ postorder right ++ [x]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears. Preorder failed check!"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears. Inorder failed check!"

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears. Postorder failed check!"


-- Write foldr for BinaryTree

-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left x right) = foldTree f leftAcc right
            where leftAcc = foldTree f (f x acc) left

testfoldTreeAdd :: IO ()
testfoldTreeAdd =
  if foldTree (+) 0 testTree == 6
  then putStrLn "foldTree (+) fine!"
  else putStrLn "Bad news bears. foldTree (+) failed check!"

testfoldTreeMult :: IO ()
testfoldTreeMult =
  if foldTree (*) 1 testTree == 6
  then putStrLn "foldTree (*) fine!"
  else putStrLn "Bad news bears. foldTree (*) failed check!"

  
main :: IO ()
main = do
  print "----- main ----------"
  mapOkay
  testPreorder
  testInorder
  testPostorder
  testfoldTreeAdd
  testfoldTreeMult
  return ()
