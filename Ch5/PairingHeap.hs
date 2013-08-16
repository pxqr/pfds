module PairingHeap where

import Control.Applicative hiding (empty)
import Data.List (unfoldr)
import Data.Monoid
import Test.QuickCheck


data Heap a = E
            | T [Heap a] a
              deriving Show

instance Ord a => Monoid (Heap a) where
  mempty  = empty
  mappend = merge

instance (Arbitrary a, Ord a) => Arbitrary (Heap a) where
  arbitrary = fromList <$> arbitrary

valid :: Ord a => Heap a -> Bool
valid  E       = True
valid (T hs x)
  | all rootIsMin hs = all valid hs
  |     otherwise    = False
  where
    rootIsMin = maybe True (x <) . findMin

empty :: Heap a
empty = E

singleton :: a -> Heap a
singleton = T []

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T hs1 x1) h2@(T hs2 x2)
  | x1 <= x2  = T (h2 : hs1) x1
  | otherwise = T (h1 : hs2) x2

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs []  = E
mergePairs [h] = h
mergePairs (h1 : h2 : hs) = merge (merge h1 h2) (mergePairs hs)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (singleton x)



findMin :: Heap a -> Maybe a
findMin  E      = Nothing
findMin (T _ a) = Just a

deleteMin :: Ord a => Heap a -> Heap a
deleteMin  E       = E
deleteMin (T hs _) = mergePairs hs

viewMin :: Ord a => Heap a -> Maybe (a, Heap a)
viewMin  E       = Nothing
viewMin (T hs x) = Just (x, mergePairs hs)


fromList :: Ord a => [a] -> Heap a
fromList = foldr insert empty

toList :: Ord a => Heap a -> [a]
toList = unfoldr viewMin

psort :: Ord a => [a] -> [a]
psort = toList . fromList

tests :: IO ()
tests = do
  quickCheck (valid :: Heap Int -> Bool)
