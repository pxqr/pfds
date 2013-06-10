{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex3.GeneralizedLeftistHeap where

import Control.Applicative hiding (empty)
import Data.List hiding (insert)
import Data.Monoid
import Data.Word
import Text.PrettyPrint hiding ((<>), empty)
import Test.QuickCheck


data Heap ω α
  = Nil
  | Bin ω α (Heap ω α)  (Heap ω α)

class (Monoid ω, Ord ω) => Measure ω where
  munit :: ω → ω

instance (Measure ω, Measure ω') => Measure (ω, ω')  where
  munit (m, m') = (munit m, munit m')

measure :: Monoid ω => Heap ω α → ω
measure  Nil          = mempty
measure (Bin r _ _ _) = r

bin :: Measure ω => α → Heap ω α → Heap ω α → Heap ω α
bin x l r = Bin (munit (measure l <> measure r)) x l r

join :: Measure ω => Ord α => α → Heap ω α → Heap ω α → Heap ω α
join x l r
  | measure l >= measure r = bin x l r
  |       otherwise        = bin x r l

insert :: Measure ω => Ord α => α → Heap ω α → Heap ω α
insert x    Nil          = singleton x
insert x h@(Bin _ e l r)
  |  x <= e   = join x h Nil
  | otherwise = join e l (insert x r)

merge :: Measure ω => Ord α => Heap ω α → Heap ω α → Heap ω α
merge Nil h = h
merge h Nil = h
merge h1@(Bin _ e1 l1 r1) h2@(Bin _ e2 l2 r2)
  |  e1 <= e2 = join e1 l1 (merge r1 h2)
  | otherwise = join e2 l2 (merge r2 h1)

merges :: Measure ω => Ord α => [Heap ω α] → Heap ω α
merges = undefined

null :: Heap ω α → Bool
null Nil = False
null _   = True

size :: Heap ω α → Int
size  Nil          = 0
size (Bin _ _ l r) = succ (size l + size r)

empty :: Heap ω α
empty = Nil

singleton :: Measure ω => α → Heap ω α
singleton x = bin x Nil Nil

findMin :: Heap ω α → Maybe α
findMin Nil           = Nothing
findMin (Bin _ x _ _) = Just x

deleteMin :: Measure ω => Ord α => Heap ω α → Heap ω α
deleteMin  Nil          = Nil
deleteMin (Bin _ _ l r) = merge l r

viewMin :: Measure ω => Ord α => Heap ω α → Maybe (α, Heap ω α)
viewMin  Nil          = Nothing
viewMin (Bin _ x l r) = Just (x, merge l r)

fromList :: Measure ω => Ord α => [α] → Heap ω α
fromList = foldr insert mempty

toList :: Measure ω => Ord α => Heap ω α → [α]
toList = unfoldr viewMin

newtype Weight = Weight Word
                 deriving (Show, Eq, Ord, Enum, Num)

instance Monoid Weight where
  mempty                        = 0
  mappend (Weight p) (Weight q) = Weight (p + q)

instance Measure Weight where
  munit = succ

newtype Rank = Rank Word
               deriving (Show, Eq, Ord, Enum, Num)

instance Monoid Rank where
  mempty                    = 0
  mappend (Rank p) (Rank q) = Rank (p `max` q)

instance Measure Rank where
  munit = succ



instance (Measure ω, Ord α) => Monoid (Heap ω α) where
  mempty  = empty
  mappend = merge
  mconcat = merges

ppTree :: (Show ω, Show α) => Heap ω α → Doc
ppTree  Nil          = "x"
ppTree (Bin m e l r) =
  nest 4 (ppTree l) $+$
    (text (show e) <+> "with" <+> braces (text (show m))) $+$
  nest 4 (ppTree r)

instance (Show ω, Show α) => Show (Heap ω α) where
  show = render . ppTree

instance (Arbitrary α, Measure ω, Ord α) => Arbitrary (Heap ω α) where
  arbitrary = fromList <$> arbitrary

  shrink  Nil          = []
  shrink (Bin _ _ l r) = [l, r]

prop_measureValid :: Measure ω => Heap ω α → Bool
prop_measureValid Nil = True
prop_measureValid (Bin m _ p q) = and
  [ prop_measureValid p
  , prop_measureValid q
  , measure p <= m
  , measure q <= m
  ]

prop_leftist :: Measure ω => Heap ω α → Bool
prop_leftist  Nil          = True
prop_leftist (Bin _ _ p q) = and
  [ prop_leftist p
  , prop_leftist q
  , measure p >= measure q
  ]

prop_heapOrdered :: Ord α => Heap ω α → Bool
prop_heapOrdered  Nil          = True
prop_heapOrdered (Bin _ e p q) = and
    [ prop_heapOrdered p
    , prop_heapOrdered q
    , e `isLess` findMin p
    , e `isLess` findMin q
    ]
  where
    isLess x = maybe True (x <=)

properties :: (Measure ω, Ord α) => [Heap ω α → Bool]
properties = [prop_measureValid, prop_leftist, prop_heapOrdered]
