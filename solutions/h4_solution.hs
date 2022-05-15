{-# LANGUAGE InstanceSigs #-}

data ErrJst e j = Err e | Jst j deriving (Show)
instance Functor (ErrJst e) where
  fmap :: (a -> b) -> ErrJst f a -> ErrJst f b
  fmap _ (Err e)  = Err e
  fmap f (Jst x) = Jst (f x)


instance Applicative (ErrJst e) where
  pure :: a -> ErrJst f a
  pure = Jst
  (<*>) :: ErrJst f (a -> b) -> ErrJst f a -> ErrJst f b  
  Err e <*> _ = Err e
  (Jst f) <*> x = fmap f x

instance Monad (ErrJst e) where
  (>>=) :: ErrJst f a -> (a -> ErrJst f b) -> ErrJst f b
  Err e >>= _ = Err e
  (Jst x) >>= f = f x

join :: Monad m => m (m a) -> m a
join m = do
  a <- m
  b <- a
  return b

data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)
instance Foldable LTree where
 foldMap :: Monoid m => (a -> m) -> LTree a -> m
 foldMap f (Leaf x) = f x
 foldMap f (LNode l r) = mappend (foldMap f l) (foldMap f r)