{-|
  Ihsan Burak Caglar
  150120057

  BLG458E - Functional Programming
  Assigment 1
  08.03.16
-}

module BinaryTree where

class BinaryTree t where
  leftTree :: t a -> Maybe (t a)
  rightTree :: t a -> Maybe (t a)
  rootValue :: t a -> Maybe a
  leaf :: a -> t a
  node :: a -> t a -> t a -> t a

data Tree1 a = Leaf1 a | Node1 a (Tree1 a) (Tree1 a)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)
