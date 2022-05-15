import Data.Char
import Parsing
import System.Environment as Env

data Prop = Const Bool
          | Var String
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Iff Prop Prop
          deriving (Eq, Show, Read)

-- Grammar G0 for propositional formulas
-- Formula ::= 'T' | 'F' | Ident
--           | '(' Formula ')'
--           | '!' Formula
--           | Formula '/\' Formula
--           | Formula '\/' Formula
--           | Formula '->' Formula
--           | Formula '<->' Formula
-- Ident starts with a lower letter followed by zero or more letters and digits
-- 'T' stands for constant True, and 'F' stands for constant False
-- Precedence (from high to low): (), !, /\, \/, ->, <->.
-- Associativity: right

-- Grammar G1 with precedence
-- Formula ::= Formula '<->' Formula | ImpTerm
-- ImpTerm ::= ImpTerm '->' ImpTerm | OrTerm
-- OrTerm  ::= OrTerm '\/' OrTerm | AndTerm
-- AndTerm ::= AndTerm '/\' AndTerm | NotTerm
-- NotTerm ::= '!' NotTerm | Factor
-- Factor  ::= '(' Formula ')' | 'T' | 'F' | Ident

-- Grammar G2 with precedence and right-associativity
-- Formula ::= ImpTerm '<->' Formula | ImpTerm
-- ImpTerm ::= OrTerm '->' ImpTerm | OrTerm
-- OrTerm  ::= AndTerm '\/' OrTerm | AndTerm
-- AndTerm ::= NotTerm '/\' AndTerm | NotTerm
-- NotTerm ::= '!' NotTerm | Factor
-- Factor  ::= '(' Formula ')' | 'T' | 'F' | Ident

constT :: Parser Prop
constT = do symbol "T"
            return (Const True)

constF :: Parser Prop
constF = do symbol "F"
            return (Const False)

constant :: Parser Prop
constant = constT <|> constF

var :: Parser Prop
var = do name <- identifier
         return (Var name)

formula :: Parser Prop
formula = do i <- impTerm
             symbol "<->"
             f <- formula
             return (Iff i f)
           <|> impTerm

impTerm :: Parser Prop
impTerm = do o <- orTerm
             symbol "->"
             i <- impTerm
             return (Imply o i)
           <|> orTerm

orTerm :: Parser Prop
orTerm = do a <- andTerm
            symbol "\\/"
            o <- orTerm
            return (Or a o)
          <|> andTerm

andTerm :: Parser Prop
andTerm = do n <- notTerm
             symbol "/\\"
             a <- andTerm
             return (And n a)
           <|> notTerm

notTerm :: Parser Prop
notTerm = do symbol "!"
             n <- notTerm
             return (Not n)
           <|> factor

factor :: Parser Prop
factor = do symbol "("
            f <- formula
            symbol ")"
            return f
          <|> constant
          <|> var

parseFormula :: String -> String
parseFormula input = case parse formula input of
                       [(p, "")] -> show p
                       [(p, _)]  -> "Parse Error"
                       []        -> "Parse Error"

main :: IO ()
main = do args <- Env.getArgs
          let filename = head args
          content <- readFile filename
          let ls = lines content
          mapM_ (putStrLn . parseFormula) ls

