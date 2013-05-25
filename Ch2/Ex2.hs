{-# LANGUAGE UnicodeSyntax #-}
module PFDS.Ch2.Ex2 where

import Control.Applicative hiding (empty)
import Test.QuickCheck


data Tree α = E
           | T (Tree α) α (Tree α)
             deriving Show

empty :: Tree α
empty = E

singleton :: α → Tree α
singleton x = T E x E

member :: Ord α => α → Tree α → Bool
member x = go
  where
    go E = False
    go (T l e r)
      |   x < e   = go l
      |   x > e   = go r
      | otherwise = True


insert :: Ord α => α → Tree α → Tree α
insert x = go
  where
    go E = singleton x
    go (T l e r)
      |   x < e   = T (go l) e r
      |   x > e   = T l      e (go r)
      | otherwise = T l      e r

{- Ex2.2 #-}
member' :: Ord α => α → Tree α → Bool
member' x = go Nothing
  where
    go (Nothing)  E        = False
    go (Just e)   E        = e == x
    go  path     (T l e r)
      |   x <= e  = go (Just e) l
      | otherwise = go  path    r

fromList :: Ord α => [α] → Tree α
fromList = foldr insert empty

{- Ex 2.3: won't do due lack of precise exceptions -}
{- Ex 2.4: the same -}

{- Ex 2.5.a -}

complete :: α → Int → Tree α
complete x = go
  where
    go 0 = empty
    go d = T sub x sub
      where
        sub = go (pred d)

{- 2.5.b -}
ofSize :: α → Int -> Tree α
ofSize _ s | s < 0 = E
ofSize x s = fst (go s)
  where
    go 0 = (E, T E x E)
    go n |   even n  = (T m' x m1', T m1' x m1')
         | otherwise = (T m x m, T m x m1)
      where
        (m', m1') = go (n `div` 2 - 1)
        (m, m1)   = go (n `div` 2)


size :: Tree α → Int
size  E        = 0
size (T l _ r) = 1 + size l + size r

instance (Arbitrary α, Ord α) => Arbitrary (Tree α) where
  arbitrary = fromList <$> arbitrary

prop_memberHas :: [Int] → Bool
prop_memberHas xs = all (`member` fromList xs) xs

prop_memberHas' :: [Int] → Bool
prop_memberHas' xs = all (`member'` fromList xs) xs

data Empty

instance Arbitrary Empty where
  arbitrary = return undefined

instance Show Empty where
  show = const "⊥"

prop_ofSize :: Empty → NonNegative Int → Bool
prop_ofSize x (NonNegative n) = size (ofSize x s) == s
  where
    s = n `mod` 1000