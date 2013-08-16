module SplayHeap where

data Heap a = Nil
              | Bin (Heap a) !a (Heap a)
                deriving Show

smaller :: Ord a => a -> Heap a -> Heap a
smaller _      Nil = Nil
smaller pivot (Bin p x q)
  | pivot < x = smaller pivot p
  | otherwise = Bin p x (smaller pivot q)

smaller' :: Ord a => a -> Heap a -> Heap a
smaller' _      Nil        = Nil
smaller' pivot (Bin p x q)
  |     pivot < x    = smaller pivot p
  |     otherwise    = go q
  where
    go  Nil          = Bin p x Nil
    go (Bin q1 qx q2)
      | pivot < qx   = Bin p x  (smaller' pivot q1)
      |   otherwise  = Bin (Bin p x q1) qx (smaller' pivot q2)

bigger :: Ord a => a -> Heap a -> Heap a
bigger _      Nil        = Nil
bigger pivot (Bin p x q)
  | x <= pivot = bigger pivot q
  | otherwise  = Bin (bigger pivot p) x q

bigger' :: Ord a => a -> Heap a -> Heap a
bigger' _      Nil    = Nil
bigger' pivot (Bin p x q)
  |     x <= pivot    = bigger pivot q
  |     otherwise     = go p
  where
    go  Nil           = Bin Nil x q
    go (Bin p1 px p2)
      |  px <= pivot  = Bin (bigger' pivot p2) x q
      |   otherwise   = Bin (bigger' pivot p1) px (Bin p2 x q)

partition :: Ord a => a -> Heap a -> (Heap a, Heap a)
partition _        Nil        = (Nil, Nil)
partition pivot t@(Bin p x q)
  | x <= pivot = goL q
  | otherwise  = goR p
  where
    goL  Nil         = (t, Nil)
    goL (Bin l y r)
      | y <= pivot   = let (sm, bg) = partition pivot r
                       in  (Bin (Bin p x l) y sm, bg)
      | otherwise    = let (sm, bg) = partition pivot l
                       in  (Bin p x sm, Bin bg y r)

    goR  Nil         = (Nil, t)
    goR (Bin l y r)
      |  y <= pivot  = let (sm, bg) = partition pivot r
                       in (Bin l y sm, Bin bg x q)
      |   otherwise  = let (sm, bg) = partition pivot l
                       in (sm, Bin (Bin bg y r) x q)


empty :: Heap a
empty = Nil

null :: Heap a -> Bool
null Nil = True
null _   = False

findMin :: Heap a -> a
findMin (Bin Nil x _) = x
findMin (Bin p   _ _) = findMin p

deleteMin :: Heap a -> Heap a
deleteMin  Nil = Nil
deleteMin (Bin Nil _ c) = c
deleteMin (Bin (Bin Nil _ b) y c) = Bin b y c
deleteMin (Bin (Bin a   x b) y c) = Bin (deleteMin a) x (Bin b y c)

insert :: Ord a => a -> Heap a -> Heap a
insert x t = Bin sm x bg
  where
    (sm, bg) = partition x t

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert empty
