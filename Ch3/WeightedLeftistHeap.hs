{-# LANGUAGE UnicodeSyntax #-}
module WeightedLeftistHeap where

import Control.Applicative hiding (empty)
import Data.Monoid
import Test.QuickCheck


type Weight = Int

data Heap α
  = Nil
  | Bin {-# UNPACK #-} !Weight α (Heap α) (Heap α)
    deriving Show

instance Ord α => Monoid (Heap α) where
  mempty  = empty
  mappend = merge
  mconcat = merges

weight :: Heap α → Weight
weight  Nil          = 0
weight (Bin r _ _ _) = r

bin :: α → Heap α → Heap α → Heap α
bin x l r
  | weight l >= weight r = Bin (succ (weight l + weight r)) x l r
  |       otherwise      = Bin (succ (weight r + weight l)) x r l



null :: Heap α → Bool
null Nil = True
null _   = False

size :: Heap α → Int
size = weight

empty :: Heap α
empty = Nil

singleton :: α → Heap α
singleton x = Bin 1 x Nil Nil

insert :: Ord α => α → Heap α → Heap α
insert x    Nil = singleton x
insert x h@(Bin w e l r)
  |   x <= e  = Bin (succ w) x h Nil
  | otherwise = bin e l (insert x r)


merge :: Ord α => Heap α → Heap α → Heap α
merge Nil h = h
merge h Nil = h
merge h1@(Bin _ e1 l1 r1) h2@(Bin _ e2 l2 r2)
  |  e1 <= e2 = bin e1 l1 (merge r1 h2)
  | otherwise = bin e2 l2 (merge r2 h1)

{- Ex3.4 (c) -}

merge' :: Ord α => Heap α → Heap α → Heap α
merge' = go id
  where
    go c Nil h = c h
    go c h Nil = c h
    go c h1@(Bin _ e1 l1 r1) h2@(Bin _ e2 l2 r2)
      | e1 <= e2  = let c' h = c $ if weight l1 < weight r1 + weight h2
                                   then Bin w e1 h l1
                                   else Bin w e1 l1 h
                    in go c' r1 h2


      | otherwise = let c' h = c $ if weight l2 < weight r2 + weight h1
                                   then Bin w e2 h l2
                                   else Bin w e2 l2 h
                    in go c' r2 h1
      where
        w = weight h1 + weight h2

{- Ex3.4 (d)

-}

merges :: Ord α => [Heap α] → Heap α
merges = foldr merge empty

findMin :: Heap α → Maybe α
findMin  Nil          = Nothing
findMin (Bin _ e _ _) = Just e

deleteMin :: Ord α => Heap α → Heap α
deleteMin  Nil          = Nil
deleteMin (Bin _ _ l r) = merge l r

fromList :: Ord α => [α] → Heap α
fromList = foldr insert empty

instance (Arbitrary α, Ord α) => Arbitrary (Heap α) where
  arbitrary = fromList <$> arbitrary

  shrink  Nil          = []
  shrink (Bin _ _ l r) = [l, r]

prop_weightValid :: Heap Int → Bool
prop_weightValid  Nil          = True
prop_weightValid (Bin w _ p q) = and
  [ prop_weightValid p
  , prop_weightValid q
  , w == weight p + weight q + 1
  ]

prop_leftist :: Heap Int → Bool
prop_leftist  Nil          = True
prop_leftist (Bin _ _ p q) = and
  [ prop_leftist p
  , prop_leftist q
  , weight p >= weight q
  ]

prop_heapOrdered :: Heap Int → Bool
prop_heapOrdered  Nil = True
prop_heapOrdered (Bin _ e p q) = and
  [ e `isLess` findMin p
  , e `isLess` findMin q
  , prop_heapOrdered p
  , prop_heapOrdered q
  ]
  where
    isLess x = maybe True (x <=)

propositionalEq :: Eq α => Heap α → Heap α → Bool
propositionalEq Nil Nil = True
propositionalEq (Bin w1 e1 l1 r1) (Bin w2 e2 l2 r2)
  | w1 == w2 && e1 == e2 = propositionalEq l1 l2 && propositionalEq r1 r2
propositionalEq _ _ = False

prop_mergeEq :: Heap Int → Heap Int → Bool
prop_mergeEq p q = merge p q `propositionalEq` merge' p q

properties = [prop_weightValid, prop_leftist, prop_heapOrdered ]
checkAll = do
  mapM_ quickCheck properties
  quickCheck prop_mergeEq