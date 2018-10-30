module Parser1 where

import Text.ParserCombinators.ReadP
import AS1
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)    

{- THE GRAMMAR:
     Expr   ::=  Aexpr | Aexpr ? Aexpr : Expr
     Aexpr  ::=  Term {{ + | - } Term}^*
     Term   ::=  Factor {{ * | / } Factor}^*
     Factor ::=  Num | (Expr) 

Note that: 1-2-3-4 = (((1-2)-3)-4), 
           1*2+3 = (1*2)+3,
           1+2*3 = 1+(2*3),
           1?2:3?4:5 = 1?2:(3?4:5), 
           etc.
-}

---------------------------------------------------------------------------
-- parser utilities

run :: ReadP a -> String -> [(a,String)]
run = readP_to_S

full :: ReadP a -> String -> Maybe a
full p inp = listToMaybe [ x | (x,"") <- run p inp]

-- Try: (aparse "1+2*3")
aparse inp = case (full expr inp) of
               Nothing -> error ("Failed to parse: "++inp)
               Just x  -> x

---------------------------------------------------------------------------
-- The parser
---------------------------------------------------------------------------
-- Standard combinators

token :: ReadP a -> ReadP a
token p    = do { result <- p ; skipSpaces ; return result }

symbol :: String -> ReadP String
symbol sym = token (string sym)

parens :: ReadP a -> ReadP a
parens p   = between (symbol "(") (symbol ")") p 

natural :: ReadP Int
natural    = do { ds <- token(munch1 isDigit) ; return (read ds :: Int) }

-- the expression parser
type AexpConstr = Aexp -> Aexp -> Aexp

expr, aexpr, term, factor, numExpr :: ReadP Aexp
plusExpr, subExpr, multExpr, divExpr :: ReadP AexpConstr
cond :: Aexp -> ReadP Aexp                                        

expr       = do { e <- aexpr
                ; option e (cond e)
                }
cond tst   = do { symbol "?"
                ; thenExpr <- aexpr
                ; symbol ":"
                ; elseExpr <- expr
                ; return (Cond tst thenExpr elseExpr)
                }
aexpr      = do { skipSpaces; (term `chainl1` (plusExpr +++ subExpr)) }
term       = factor `chainl1` (multExpr +++ divExpr)
factor     = numExpr +++ parens(expr)

plusExpr   = do { symbol "+";   return Add     }
subExpr    = do { symbol "-";   return Sub     }
multExpr   = do { symbol "*";   return Mult    }
divExpr    = do { symbol "/";   return Div     }
numExpr    = do { n <- natural; return (Num n) }
