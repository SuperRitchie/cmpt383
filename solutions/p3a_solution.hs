import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

data Type = TInt
          | TBool
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
          | Abs VarId Type Expr
          | App Expr Expr
          | LetIn VarId Type Expr Expr
          deriving (Eq, Ord, Read, Show)

type Env = Map.Map VarId Type

typingArith :: Maybe Type -> Maybe Type -> Maybe Type
typingArith t1 t2 = case (t1, t2) of
                      (Just TInt, Just TInt) -> Just TInt
                      _                      -> Nothing

typingEq :: Maybe Type -> Maybe Type -> Maybe Type
typingEq t1 t2 = case (t1, t2) of
                   (Just TInt,  Just TInt)  -> Just TBool
                   (Just TBool, Just TBool) -> Just TBool
                   _                        -> Nothing

retIfEq :: Maybe Type -> Maybe Type -> Maybe Type
retIfEq t1 t2 = if t1 == t2 then t1 else Nothing

typing :: Env -> Expr -> Maybe Type
typing g (CInt _)          = Just TInt
typing g (CBool _)         = Just TBool
typing g (Var x)           = Map.lookup x g
typing g (Plus e1 e2)      = typingArith (typing g e1) (typing g e2)
typing g (Minus e1 e2)     = typingArith (typing g e1) (typing g e2)
typing g (Equal e1 e2)     = typingEq (typing g e1) (typing g e2)
typing g (ITE e1 e2 e3)    = case typing g e1 of
                               Just TBool -> retIfEq (typing g e2) (typing g e3)
                               _          -> Nothing
typing g (Abs x t e)       = fmap (TArr t) (typing (Map.insert x t g) e)
typing g (App e1 e2)       = case typing g e1 of
                               Just (TArr t1 t2) -> case typing g e2 of
                                                      Just t3 -> if t1 == t3
                                                                 then Just t2
                                                                 else Nothing
                                                      _       -> Nothing
                               _                 -> Nothing

typing g (LetIn x t e1 e2) = case typing g e1 of
                               Just t1 -> if t == t1
                                          then typing (Map.insert x t g) e2
                                          else Nothing
                               _       -> Nothing

readExpr :: String -> Expr
readExpr = read

typeCheck :: Expr -> String
typeCheck e = case typing Map.empty e of
  Just t  -> show t
  Nothing -> "Type Error"

main :: IO ()
main = do args <- getArgs
          let filename = head args
          content <- readFile filename
          let ls = lines content
          mapM_ (putStrLn . typeCheck . readExpr) ls

