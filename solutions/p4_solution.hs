import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Environment (getArgs)
import Control.Monad.State.Lazy

data Type = TInt
          | TBool
          | TError
          | TVar Int
          | TArr Type Type
          deriving (Eq, Ord, Read, Show)

type VarId = String

data Expr = CInt Int
          | CBool Bool
          | Var VarId
          | Plus Expr Expr
          | Minus Expr Expr
          | Equal Expr Expr
          | ITE Expr Expr Expr
          | Abs VarId Expr
          | App Expr Expr
          | LetIn VarId Expr Expr
          deriving (Eq, Ord, Read, Show)

type Env = Map.Map VarId Type

data Constraint = CEq Type Type
                | CError
                deriving (Eq, Ord, Read, Show)

type ConstraintSet = Set.Set Constraint
type ConstraintList = [Constraint]

type InferState a = State Int a

getFreshTVar :: InferState Type
getFreshTVar = do x <- get
                  put (x + 1)
                  return (TVar x)

infer :: Env -> Expr -> InferState (Type, ConstraintSet)
infer g (CInt _)        = return (TInt, Set.empty)
infer g (CBool _)       = return (TBool, Set.empty)
infer g (Var x)         = case Map.lookup x g of
                            Just t  -> return (t, Set.empty)
                            Nothing -> return (TError, Set.singleton CError)
infer g (Plus e1 e2)    = do (t1, c1) <- infer g e1
                             (t2, c2) <- infer g e2
                             let c = Set.unions [c1, c2, Set.fromList [CEq t1 TInt, CEq t2 TInt]]
                             return (TInt, c)
infer g (Minus e1 e2)   = do (t1, c1) <- infer g e1
                             (t2, c2) <- infer g e2
                             let c = Set.unions [c1, c2, Set.fromList [CEq t1 TInt, CEq t2 TInt]]
                             return (TInt, c)
infer g (Equal e1 e2)   = do (t1, c1) <- infer g e1
                             (t2, c2) <- infer g e2
                             let c = Set.unions [c1, c2, Set.singleton (CEq t1 t2)]
                             return (TBool, c)
infer g (ITE e1 e2 e3)  = do (t1, c1) <- infer g e1
                             (t2, c2) <- infer g e2
                             (t3, c3) <- infer g e3
                             let c = Set.unions [c1, c2, c3, Set.fromList [CEq t1 TBool, CEq t2 t3]]
                             return (t2, c)
infer g (Abs x e)       = do y <- getFreshTVar
                             (t, c) <- infer (Map.insert x y g) e
                             return (TArr y t, c)
infer g (App e1 e2)     = do x1 <- getFreshTVar
                             x2 <- getFreshTVar
                             (t1, c1) <- infer g e1
                             (t2, c2) <- infer g e2
                             let c = Set.unions [c1, c2, Set.fromList [CEq t1 (TArr x1 x2), CEq t2 x1]]
                             return (x2, c)
infer g (LetIn x e1 e2) = do y <- getFreshTVar
                             (t1, c1) <- infer (Map.insert x y g) e1
                             (t2, c2) <- infer (Map.insert x y g) e2
                             let c = Set.unions [c1, c2, Set.singleton (CEq y t1)]
                             return (t2, c)

inferExpr :: Expr -> (Type, ConstraintSet)
inferExpr e = evalState (infer Map.empty e) 1

toCstrList :: ConstraintSet -> ConstraintList
toCstrList = Set.toList

type Substitution = Map.Map Type Type

applySub :: Substitution -> Type -> Type
applySub s TInt         = TInt
applySub s TBool        = TBool
applySub s TError       = TError
applySub s (TVar x)     = case Map.lookup (TVar x) s of
                            Just t  -> t
                            Nothing -> TVar x
applySub s (TArr t1 t2) = TArr (applySub s t1) (applySub s t2)

applySubToCstr :: Substitution -> Constraint -> Constraint
applySubToCstr s c = case c of
                       CEq t1 t2 -> CEq (applySub s t1) (applySub s t2)
                       CError    -> CError

applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
applySubToCstrList s l = map (applySubToCstr s) l

composeSub :: Substitution -> Substitution -> Substitution
composeSub s1 s2 = Map.union (Map.map (applySub s1) s2)
                             (Map.filterWithKey (\k _ -> Map.notMember k s2) s1)

tvars :: Type -> Set.Set Type
tvars TInt         = Set.empty
tvars TBool        = Set.empty
tvars TError       = Set.empty
tvars (TVar x)     = Set.singleton (TVar x)
tvars (TArr t1 t2) = Set.union (tvars t1) (tvars t2)

unify :: ConstraintList -> Maybe Substitution
unify []       = Just Map.empty
unify (c:tail) =
  case c of
    CError  -> Nothing
    CEq s t -> if s == t
               then unify tail
               else case (s, t) of
                      (TVar x, _) -> unifyOneVar tail (TVar x) t
                      (_, TVar y) -> unifyOneVar tail (TVar y) s
                      (TArr s1 s2, TArr t1 t2) -> unify (tail ++ [CEq s1 t1, CEq s2 t2])
                      _           -> Nothing

unifyOneVar :: ConstraintList -> Type -> Type -> Maybe Substitution
unifyOneVar l (TVar x) t = if Set.notMember (TVar x) (tvars t)
                           then do let sub = Map.singleton (TVar x) t
                                   s1 <- unify (applySubToCstrList sub l)
                                   return (composeSub s1 sub)
                           else Nothing
unifyOneVar _ _ _ = Nothing

typing :: Expr -> Maybe Type
typing e = do let (t, c) = inferExpr e
              s <- unify (toCstrList c)
              return (applySub s t)

-------------------- Type Var Relabeling ---------------------
type RelabelState a = State (Map.Map Int Int) a

relabel :: Type -> Type
relabel t = evalState (go t) Map.empty
  where
    go :: Type -> RelabelState Type
    go TInt         = return TInt
    go TBool        = return TBool
    go TError       = return TError
    go (TVar x)     = do m <- get
                         case Map.lookup x m of
                           Just v  -> return (TVar v)
                           Nothing -> do let n = 1 + Map.size m
                                         put (Map.insert x n m)
                                         return (TVar n)
    go (TArr t1 t2) = do t1' <- go t1
                         t2' <- go t2
                         return (TArr t1' t2')
---------------------------------------------------------------

readExpr :: String -> Expr
readExpr = read

typeInfer :: Expr -> String
typeInfer e = case typing e of
                Just t  -> show (relabel t)
                Nothing -> "Type Error"

main :: IO ()
main = do args <- getArgs
          let filename = head args
          content <- readFile filename
          let ls = lines content
          mapM_ (putStrLn . typeInfer . readExpr) ls

