module Lib where

addMod :: Int -> Int -> Int -> Int
addMod x y m = (x + y) `mod` m

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)
