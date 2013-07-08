{-# LANGUAGE UnicodeSyntax #-}
module Ch3.BinomialHeap where


type Rank = Int

data Tree α = Node {
    rank     :: {-# UNPACK #-} !Rank
  , elem     :: α
  , children :: [Tree α]
  }

type Heap α = [Tree α]

link :: Ord α => Tree α → Tree α → Tree α
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  |  x1 < x2  = Node (succ r) x1 (t2 : c1)
  | otherwise = Node (succ r) x2 (t1 : c2)

insTree :: Ord α => Tree α → Heap α → Heap α
insTree t [] = [t]
insTree t ts@(t' : ts')
  | rank t < rank t' = t : ts
  |     otherwise    = insTree (link t t') ts'

merge :: Ord α => Heap α → Heap α → Heap α
merge [] t = t
merge t [] = t
merge t1@(t : ts) t2 = undefined
