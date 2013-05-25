{-# LANGUAGE UnicodeSyntax, EmptyDataDecls #-}
module PFDS.Ch2.Ex1 where

import Test.QuickCheck


suffixes :: [α] → [[α]]
suffixes [] = [[]]
suffixes xxs@(_ : xs) = xxs : suffixes xs

{- Resulting list of suffixes is generated in O(n) time:

   base: [[]] takes  time;
   step: (xxs : _) takes constant time;

   Thus list of length n takes n steps each of which takes O(1) time;

-- TODO: show that it takes O(n) space
 -}

data Empty

instance Show Empty where
  show _ = "⊥"

instance Arbitrary Empty where
  arbitrary = return undefined

prop_spineLen :: [Empty] -> Bool
prop_spineLen xs = length (suffixes xs) == succ (length xs)

prop_decLen :: [Empty] -> Bool
prop_decLen = checkLens . suffixes
  where
    checkLens [ ] = True
    checkLens [_] = True
    checkLens (a : b : xs)
      | length a > length b = checkLens (b : xs)
      |      otherwise      = False

prop_all :: [Empty] -> Bool
prop_all xs = and [prop_spineLen xs, prop_decLen xs]