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
typingArith m1 m2 = do t1 <- m1
                       t2 <- m2
                       case (t1, t2) of
                         (TInt, TInt) -> return TInt
                         _            -> Nothing

typingEq :: Maybe Type -> Maybe Type -> Maybe Type
typingEq m1 m2 = do t1 <- m1
                    t2 <- m2
                    case (t1, t2) of
                      (TInt,  TInt)  -> return TBool
                      (TBool, TBool) -> return TBool
                      _              -> Nothing

typing :: Env -> Expr -> Maybe Type
typing g (CInt _)          = Just TInt
typing g (CBool _)         = Just TBool
typing g (Var x)           = Map.lookup x g
typing g (Plus e1 e2)      = typingArith (typing g e1) (typing g e2)
typing g (Minus e1 e2)     = typingArith (typing g e1) (typing g e2)
typing g (Equal e1 e2)     = typingEq (typing g e1) (typing g e2)
typing g (ITE e1 e2 e3)    = do t1 <- typing g e1
                                t2 <- typing g e2
                                t3 <- typing g e3
                                if t1 == TBool && t2 == t3 then return t2 else Nothing
typing g (Abs x t1 e)      = do t2 <- typing (Map.insert x t1 g) e
                                return (TArr t1 t2)
typing g (App e1 e2)       = do t1 <- typing g e1
                                t2 <- typing g e2
                                case t1 of
                                  TArr t3 t4 -> if t2 == t3 then return t4 else Nothing
                                  _          -> Nothing
typing g (LetIn x t e1 e2) = do t1 <- typing g e1
                                t2 <- typing (Map.insert x t g) e2
                                if t == t1 then return t2 else Nothing

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

