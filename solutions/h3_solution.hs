-- Question 1
data List a = Empty
            | Cons a (List a)
            deriving (Show)

listZip :: List a -> List b -> List (a, b)
listZip _ Empty = Empty
listZip Empty _ = Empty
listZip (Cons x xs) (Cons y ys) = Cons (x, y) (listZip xs ys)

-- Question 2
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
            deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert v EmptyTree = Node v EmptyTree EmptyTree
insert v (Node x left right)
  | v < x  = Node x (insert v left) right
  | v > x  = Node x left (insert v right)

-- Question 3
data Nat = Zero
         | Succ Nat
         deriving (Show)

natPlus :: Nat -> Nat -> Nat
natPlus Zero n = n
natPlus (Succ m) n = Succ (natPlus m n)

natMult :: Nat -> Nat -> Nat
natMult Zero n = Zero
natMult (Succ m) n = natPlus (natMult m n) n

-- Question 4
instance Eq a => Eq (Tree a) where
  EmptyTree    == EmptyTree     = True
  Node x xl xr == Node y yl yr  = x == y && xl == yl && xr == yr
  _            == _             = False

-- Question 5
data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

instance Functor (AssocList k) where
  fmap :: (a -> b) -> AssocList k a -> AssocList k b
  fmap _ ALEmpty = ALEmpty
  fmap f (ALCons x y tail) = ALCons x (f y) (fmap f tail)

