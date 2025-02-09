module Main (main) where
import Test.QuickCheck
import Lib

prop_addMod_correct :: Int -> Int -> Positive Int -> Bool
prop_addMod_correct x y (Positive m) = addMod x y m == (x + y) `mod` m
prop_addMod_identity :: Int -> Positive Int -> Bool
prop_addMod_identity x (Positive m) = addMod x 0 m == x `mod` m
prop_addMod_commutative :: Int -> Int -> Positive Int -> Bool
prop_addMod_commutative x y (Positive m) = addMod x y m == addMod y x m

genTree :: Arbitrary a => Gen (Tree a)
genTree = frequency [(1, return Empty), (3, Node <$> arbitrary <*> genTree <*> genTree)]
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = genTree

prop_treeDepth_empty :: Bool
prop_treeDepth_empty = treeDepth (Empty :: Tree Int) == 0
prop_treeDepth_singleNode :: Int -> Bool
prop_treeDepth_singleNode x = treeDepth (Node x Empty Empty) == 1
prop_treeDepth_maxDepth :: Tree Int -> Tree Int -> Int -> Bool
prop_treeDepth_maxDepth left right x =
    treeDepth (Node x left right) == 1 + max (treeDepth left) (treeDepth right)
prop_treeDepth_insert :: Tree Int -> Int -> Bool
prop_treeDepth_insert tree x = treeDepth (Node x tree Empty) >= treeDepth tree

main :: IO ()
main = do
    quickCheck prop_addMod_correct
    quickCheck prop_addMod_identity
    quickCheck prop_addMod_commutative
    quickCheck prop_treeDepth_empty
    quickCheck prop_treeDepth_singleNode
    quickCheck prop_treeDepth_maxDepth