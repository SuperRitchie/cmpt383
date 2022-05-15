import qualified Data.Map.Strict as Map
import qualified System.Environment as Env

data Formula = Const Bool
          | Var String
          | And Formula Formula
          | Or Formula Formula
          | Not Formula
          | Imply Formula Formula
          | Iff Formula Formula
          deriving (Show, Read)

type VarId  = String
type VarAsgn = Map.Map VarId Bool

findVarIds' :: Formula -> [VarId]
findVarIds' (Const _)     = []
findVarIds' (Var x)       = [x]
findVarIds' (And   p1 p2) = findVarIds p1 ++ findVarIds p2
findVarIds' (Or    p1 p2) = findVarIds p1 ++ findVarIds p2
findVarIds' (Not   p)     = findVarIds p
findVarIds' (Imply p1 p2) = findVarIds p1 ++ findVarIds p2
findVarIds' (Iff   p1 p2) = findVarIds p1 ++ findVarIds p2

-- Should import Data.List (nub)
-- This nub does not preserve the order
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs)
  | elem x xs = nub xs
  | otherwise = x : nub xs

findVarIds :: Formula -> [VarId]
findVarIds = nub . findVarIds'

genVarAsgns :: [VarId] -> [VarAsgn]
genVarAsgns []     = [Map.empty]
genVarAsgns (x:xs) = let tailAsgns = genVarAsgns xs
                     in concat $ map (\env -> [Map.insert x True env, Map.insert x False env]) tailAsgns

eval :: Formula -> VarAsgn -> Bool
eval (Const True)  _ = True
eval (Const False) _ = False
eval (Var   x)     m = case Map.lookup x m of
                         Just b  -> b
                         Nothing -> error $ show x ++ " is not in map"
eval (And   p1 p2) m = eval p1 m && eval p2 m
eval (Or    p1 p2) m = eval p1 m || eval p2 m
eval (Not   p)     m = not $ eval p m
eval (Imply p1 p2) m = not (eval p1 m) || eval p2 m
eval (Iff   p1 p2) m = eval p1 m == eval p2 m

sat :: Formula -> Bool
sat p = or . map (eval p) . genVarAsgns . findVarIds $ p

resToStr :: Bool -> String
resToStr b = case b of
  True  -> "SAT"
  False -> "UNSAT"

main :: IO ()
main = do
  args <- Env.getArgs
  let filename = head args
  content <- readFile filename
  let formulas = map read (lines content) :: [Formula]
  mapM_ (putStrLn . resToStr . sat) formulas

