--
--          Francisco Samuel Rios
--         COP 4020 November 12, 2015
--

module ExpressionsParser where

import Parser
import Expressions
import Data.List

parser :: Parse Char Expr

-- I'm embarassed by how long this took me. Ashamed, even. You were right, Data.Lists was a MUCH more sound use of my time
-- Tab, newline, and space were adequate for this, correct?
parser = (litParse `alt` varParse `alt` opExpParse) . filter (\chr -> not (chr `elem` [' ', '\t', '\n']))

opExpParse :: Parse Char Expr
opExpParse
  = (
      token  '('  >*>
      parser      >*>
      spot   isOp >*>
      parser      >*>
      token  ')'
    ) `build` makeExpr

makeExpr :: (Char, (Expr, (Char, (Expr, Char)))) -> Expr
makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

litParse :: Parse Char Expr
litParse 
  = (
      (optional (token '-')) >*>
      (neList (spot isDigit))
    ) `build` (charlistToExpr . uncurry (++))
varParse :: Parse Char Expr
varParse = spot isVar `build` Var

isVar :: Char -> Bool
isVar x = ('a' <= x && x <= 'z')

charToOp :: Char -> Ops
charToOp '+' = Add
charToOp '-' = Sub
charToOp '*' = Mul
charToOp '/' = Div

isOp :: Char -> Bool
isOp '+' = True
isOp '-' = True
isOp '*' = True
isOp '/' = True
isOp _   = False

charlistToExpr :: String -> Expr
charlistToExpr str = Lit (read str)