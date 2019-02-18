module BinaryTree where

data BinaryTree a = 
    Leaf 
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = 
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = 
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left a right) = a : preOrder left ++ preOrder right

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left a right) = inOrder left ++ [a] ++ inOrder right

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left a right) = postOrder left ++ postOrder right ++ [a]

testTree :: BinaryTree Integer
testTree = 
  Node (Node Leaf 1 Leaf)
       2 
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preOrder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inOrder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postOrder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z Leaf = z
foldTree f z (Node left a right) = f a (foldTree f (foldTree f z left) right)

fT :: (a -> b -> b) -> b -> BinaryTree a -> b
fT f z Leaf = z
fT f z (Node left a right) =fT f a (fT z left) right
