{-# LANGUAGE InstanceSigs #-}

data ErrJst e j = Err e | Jst j deriving (Show)
instance Functor (ErrJst e) where
  fmap :: (a -> b) -> ErrJst j a -> ErrJst j b
  fmap _ (Err e)  = Err e
  fmap f (Jst x) = Jst (f x)

main = do
  print(fmap (+1) (Err 1))