-- Version 1/1, Fri Feb 23 11:44:04 EST 2018
module LCParserP where

-- import qualified Text.ParserCombinators.ReadP as R
import Text.ParserCombinators.ReadP
import Data.Char (isDigit,isSpace)
import Data.Maybe (listToMaybe)    
import Test.QuickCheck

import LC

{- THE GRAMMAR:
     Loc    ::=  x0 | x1 | x2 | ...
     
     Expr   ::=  Term {{ + | - } Term}^*
     Term   ::=  Factor {* Factor}^*
     Factor ::=  (Expr) | Num | val(Loc)

     Bexpr  ::=  ff | tt | Expr Cop Expr | (Bexpr)
     Cop    ::=  == | /= | < | <= | > | >=
  
     Cmd    ::= skip
              | { } 
              | { Cmd [; Cmd]^* }
              | Loc := Expr
              | if Bexpr then Cmd else Cmd
              | while Bexpr do Cmd
              | do Cmd whilest Bexpr
-}


---------------------------------------------------------------------------
-- The parser
---------------------------------------------------------------------------
-- utilities 


run :: ReadP a -> String -> [(a,String)]
run = readP_to_S
      
full :: ReadP a -> String -> Maybe a
full p inp = listToMaybe [ x | (x,"") <- run p inp]

token :: ReadP a -> ReadP a
token p    = do { result <- p ; skipSpaces ; return result }

symbol :: String -> ReadP String
symbol sym = token (string sym)

parens :: ReadP a -> ReadP a
parens p   = between (symbol "(") (symbol ")") p 

braces :: ReadP a -> ReadP a
braces p   = between (symbol "{") (symbol "}") p 

natural :: ReadP Int
natural    = do { ds <- token(munch1 isDigit) ; return (read ds :: Int) }

integer :: ReadP Int
integer    = do { char '-'; n <- natural; return (-n) } +++ natural

semiSep :: ReadP a -> ReadP [a]
semiSep p  = sepBy p (symbol ";")


-- locations
location   = do { char 'x'; n <- natural; return (fromIntegral n) }

-- arithmetic expressions
expr       = term   `chainl1` (plusExpr +++ subExpr)
term       = factor `chainl1` multExpr
factor     = valExpr +++ numExpr +++ parens(expr)

plusExpr   = do { symbol "+";  return (ABin Plus)  }
subExpr    = do { symbol "-";  return (ABin Minus) }
multExpr   = do { symbol "*";  return (ABin Times) }
numExpr    = do { n <- integer; return (AConst (fromIntegral n)) }
valExpr    = do { symbol "val"; n <- parens(location); return (Val n) }

-- boolean expressions
bexpr     = parens(bexpr) +++ ttExpr +++ ffExpr +++ compExpr  
ttExpr    = do { symbol "tt"; return (BConst True) }
ffExpr    = do { symbol "ff"; return (BConst False) }
compExpr  = do { e1 <- expr
               ; cop <- cOp
               ; e2 <- expr
               ; return (Compare cop e1 e2)
               }

cOp       = choice [ cOper "==" Eq  , cOper "/=" Neq , cOper "<" Lt   , 
                     cOper "<=" Leq , cOper ">" Gt   , cOper ">=" Geq ]
    where
      cOper st op = do { symbol st; return op }

-- commands
cmd       = do { skipSpaces
               ; choice [skipCmd, ifCmd, whileCmd, seqCmd, assgnCmd, doWhilstCmd]
               }

skipCmd   = do { symbol "skip" ; return Skip }

ifCmd     = do { symbol "if"
               ; tst <- bexpr
               ; symbol "then"
               ; thenCmd <- cmd
               ; symbol "else"
               ; elseCmd <- cmd
               ; return (If tst thenCmd elseCmd)
               }

whileCmd  = do { symbol "while"
               ; tst <- bexpr
               ; symbol "do"
               ; body <- cmd
               ; return (While tst body)
               }

doWhilstCmd = do { symbol "do"
               ; body <- cmd
               ; symbol "whilst"
               ; tst <- bexpr
               ; return (DoWhilst body tst)
               }

-- seq ::= { cmd (; cmd)^* }
seqCmd    = do { ss <- braces(semiSep cmd)
               ; if null ss then return Skip else return (foldr1 Seq ss) 
               }

assgnCmd  = do { n <- location
               ; symbol ":="
               ; rhs <- expr;
               ; return (Set n rhs)
               }

---------------------------------------------------------------------------
-- top level test functions

-- -- parseWith :: ReadP a -> String -> a
-- -- stolen from http://cmsc-16100.cs.uchicago.edu/2014/Lectures/lecture-20.php
-- parseWith p s
--     = case [a | (a,t) <- parse p' s, all isSpace t] of
--         [a] -> a
--         []  -> error "no parse"
--         _   -> error "ambiguous parse"   
--       where p' = do { skipSpaces; p }


-- (aparse inp) = the parse of an arithmetic expression 
aparse inp = case (full (do { skipSpaces; expr}) inp) of
               Nothing     -> error ("Failed to parse " ++ inp)
               Just result -> result

-- (bparse inp) = the parse of a boolean expression 
bparse inp = case (full (do { skipSpaces; bexpr}) inp) of
               Nothing     -> error ("Failed to parse " ++ inp)
               Just result -> result

-- (cparse inp) = the parse of a statement 
cparse inp = case (full (do { skipSpaces; cmd}) inp) of
               Nothing     -> error ("Failed to parse " ++ inp)
               Just result -> result

-- QuickCheck property, try
--    quickCheck prop_lcp
prop_lcp cmd = (cmd == cparse (show cmd))


fact' = "{ x1 := 1; x2 := val(x0); while (val(x2)>0) do { x1 := (val(x1)*val(x2)); x2 := (val(x2)-1) } }"

fact4 = "{ x1 := 1; x2 := 4; do { x1 := val(x1)*val(x2); x2 := val(x2)-1 }"
        ++ " whilst val(x2)>=1 }"
        


-- ---------------------------------------------------------------------------

-- --  S ::= a(&a)^*

-- {-
-- data Atree = AnA | Node Atree Atree
--              deriving (Show)

-- sillyl = do { skipSpaces
--             ; getA `chainl1` getEt
--             }
-- sillyr = do { skipSpaces
--             ; getA `chainr1` getEt
--             }

-- getA  = do { P.symbol "a"; return AnA }
-- getEt = do { P.symbol "&"; return Node }


-- -}
