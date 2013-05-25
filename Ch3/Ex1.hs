{-# LANGUAGE UnicodeSyntax #-}
module Ch3.Ex1 where

import Control.Applicative hiding (empty)
import Data.Monoid
import Test.QuickCheck


type Rank = Int

data Heap α = Nil
            | Bin Rank α (Heap α) (Heap α)
              deriving Show

instance Ord α => Monoid (Heap α) where
  mempty  = empty
  mappend = merge

empty :: Heap α
empty = Nil

singleton :: α → Heap α
singleton x = Bin 1 x Nil Nil


merge :: Ord α => Heap α → Heap α → Heap α
merge Nil h   = h
merge h   Nil = h
merge h1@(Bin _ e1 l1 r1) h2@(Bin _ e2 l2 r2)
  |  e1 <= e2 = bin e1 l1 (merge r1 h2)
  | otherwise = bin e2 l2 (merge r2 h1)

insert :: Ord α => α → Heap α → Heap α
insert = merge . singleton

findMin :: Ord α => Heap α → Maybe α
findMin  Nil          = Nothing
findMin (Bin _ x _ _) = Just x

deleteMin :: Ord α => Heap α → Heap α
deleteMin  Nil          = Nil
deleteMin (Bin _ _ l r) = merge l r

fromList :: Ord α => [α] → Heap α
fromList = foldr insert empty

rank :: Heap α → Rank
rank  Nil          = 0
rank (Bin r _ _ _) = r

bin :: α → Heap α → Heap α → Heap α
bin x l r
  | rank l >= rank r = Bin (rank l + 1) x l r
  |    otherwise     = Bin (rank r + 1) x r l


instance (Arbitrary α, Ord α) => Arbitrary (Heap α) where
  arbitrary = fromList <$> arbitrary
  shrink    Nil = []
  shrink   (Bin _ _ l r) = [l, r]

prop_rankValid :: Heap Int -> Bool
prop_rankValid  Nil          = True
prop_rankValid (Bin r _ p q) = and
  [ r == succ (rank p)
  , prop_rankValid p
  , prop_rankValid q
  ]

prop_leftist :: Heap Int → Bool
prop_leftist  Nil          = True
prop_leftist (Bin r _ p q) = and
  [ prop_leftist p
  , prop_leftist q
  , rank p >= rank q
  ]

tests = [prop_rankValid, prop_leftist]

check_all :: IO ()
check_all = mapM_ quickCheck tests
