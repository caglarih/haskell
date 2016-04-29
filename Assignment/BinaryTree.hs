{-|
  Ihsan Burak Caglar
  150120057

  BLG458E - Functional Programming
  Assigment 3
  29.04.16
-}

module BinaryTree where

import           Data.Maybe

class BinaryTree t where
  leftTree :: t a -> Maybe (t a)
  rightTree :: t a -> Maybe (t a)
  rootValue :: t a -> Maybe a
  leaf :: a -> t a
  node :: a -> t a -> t a -> t a

data Tree1 a = Leaf1 a | Node1 a (Tree1 a) (Tree1 a)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)

instance BinaryTree Tree1 where
  leftTree t = case t of
    Leaf1 _     -> Nothing
    Node1 _ l _ -> Just l
  rightTree t = case t of
    Leaf1 _     -> Nothing
    Node1 _ _ r -> Just r
  rootValue t = case t of
    Leaf1 n     -> Just n
    Node1 n _ _ -> Just n
  leaf = Leaf1
  node = Node1

instance BinaryTree Tree2 where
  leftTree t = case t of
    Leaf2 _   -> Nothing
    Node2 l _ -> Just l
  rightTree t = case t of
    Leaf2 _   -> Nothing
    Node2 _ r -> Just r
  rootValue t = case t of
    Leaf2 n   -> Just n
    Node2 _ _ -> Nothing
  leaf = Leaf2
  node _ = Node2

isLeaf :: BinaryTree t => t a -> Bool
isLeaf t = isNothing (leftTree t)


isBranching :: BinaryTree t => t a -> Bool
isBranching t = isJust (leftTree t)

preorder :: BinaryTree t => t a -> [a]
preorder = undefined
--preorder t
--  | isLeaf t  = [fromJust(rootValue t)]
--  | otherwise = fromJust(rootValue t) : preorder (fromJust(leftTree t)) ++ preorder (fromJust(rightTree t))

inorder :: BinaryTree t => t a -> [a]
inorder = undefined

postorder :: BinaryTree t => t a -> [a]
postorder = undefined

trimLeaves :: BinaryTree t => t a -> t a
trimLeaves = undefined
