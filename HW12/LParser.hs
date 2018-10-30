-- CIS 352: Homework 12, version 4.2 (13 April 2018)
-- Written by: Xiaomeng Cao

module LParser where

import Text.ParserCombinators.ReadP
import Data.Char
import Test.QuickCheck

import LL

{- THE GRAMMAR (This is equivalent to the grammar in the handout.)
     LOWER       ::=  all lower case letters
     NOTX        ::=  all lower case letters but x
     UPPER       ::=  all upper case letters

     VAR         ::=  x0 | x1 | ...
     CONST       ::=  NOTX LOWER^*
     INDIV       ::=  VAR | CONST

     RELNAME     ::=  UPPER LOWER^* 
     
     EXPR        ::=  TERM   { + TERM }^*
     TERM        ::=  FACTOR { & FACTOR }^*
     FACTOR      ::=  NEGEXPR | FORALLEXPR | EXISTSEXPR | ATOM
     NEGEXPR     ::=  ~ATOM
     FORALLEXPR  ::=  Forall VAR (EXPR)
     EXISTSEXPR  ::=  Exists VAR (EXPR)
     RELEXPR     ::=  RELNAME[INDIV {,INDIV}^*] 
     ATOM        ::=  RELEXPR | (EXPR)

   Note: There are no set braces (or ``^*'') in the language.  
   ``{'' and ``}'' are grammatical brackets.  E.g., in
     EXPR        ::=  TERM   { + TERM }^*   
   the { }^* is short hand for 
     EXPR        ::=  TERM   TERMS
     TERMS       ::=  + TERM TERMS | epsilon
   and similarly in the TERM and RELEXPR rules.
-}

fixMe = error "Please fix me"

-- (parse p inp) runs parser p on input string inp and
--   returns the list of parses
parse :: ReadP a -> String -> [(a,String)]
parse = readP_to_S


parseWith :: ReadP a -> String -> a
-- stolen from http://cmsc-16100.cs.uchicago.edu/2014/Lectures/lecture-20.php
parseWith p s
    = case [a | (a,t) <- parse p' s, all isSpace t] of
        [a] -> a
        []  -> error "no parse"
        _   -> error "ambiguous parse"
      where p' = do { skipSpaces; p } -- skips initial whitespace

answer :: ReadP a -> String -> a
answer = parseWith


-- a greedy version of chainl (might be handy)
chainltoo :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainltoo p op x = chainl1 p op <++ return x

-- A greedy version of chainl1 (might be handy)
chainl1too :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1too p op = p >>= rest
  where rest x = do { f <- op; y <- p; rest (f x y) }
                 <++ return x
---------------------------------------------------------------------------
-- The parser
---------------------------------------------------------------------------
-- general utilities

-- run parser p, skip any white space, and return p's result
token :: ReadP a -> ReadP a
token p    = do { result <- p ; skipSpaces ; return result }

-- finds and returns a particular string while skiping any whitespace after
-- the string.  E.g.,
--   (parse (symbol "while") "while  (val(x0)>0)  do ...")
-- returns
--   [("while","(val(x0)>0)  do ...")]
symbol :: String -> ReadP String
symbol sym = token (string sym)

parens :: ReadP a -> ReadP a
parens p   = between (symbol "(") (symbol ")") p 

brackets :: ReadP a -> ReadP a
brackets p = between (symbol "[") (symbol "]") p 

natural :: ReadP Int
natural    = do { ds <- token(munch1 isDigit) ; return (read ds :: Int) }

---------------------------------------------------------------------------
-- parsers for LL 

-- VAR :: x0 | x1 | ...
var        = do { char 'x'; n <- natural; return (Var (fromIntegral n)) }

-- CONST :: NOTX LOWER^*
constant   = do { first <- satisfy notX
                ; rest  <- munch isLetter
                ; return $ Const (first:rest)
                }
    where notX c = (c/='x') && isLetter c

-- INDIV :: VAR | CONST
indiv      = var <++ constant

-- RELNAME ::= UPPER LOWER^*
--   (answer relName "Foo") should return "Foo"
--   (answer relName "foo") should fail
relName :: ReadP String
relName    = do { first <- satisfy isUpper
                ; rest <- munch isLetter
                ; return (first:rest)
                }
             +++
             do { first <- satisfy isLower
                ; rest <- munch isLetter
                ; return ("fail")
                }

-- RELEXPR     ::=  RELNAME[INDIV {,INDIV}^*]
--   (answer relExpr "Foo[boo]") should return:
--        Pred "Foo" [Const "boo"]
--   (answer relExpr "Foo[boo,moo]") should return:
--        Pred "Foo" [Const "boo", Const "moo"]
--   (answer relExpr "Foo[boo,x11]") should return:
--        Pred "Foo" [Const "boo", Var 11]
--   (answer relExpr "Foo[]") should fail.
relExpr    = fixMe
                          
-- ATOM        ::=  RELEXPR | (EXPR)
atom       = fixMe

-- FORALLEXPR  ::=  Forall VAR (EXPR)
forAllExpr = fixMe

-- Note: The abstract syntax of "Forall x12 (R[x12,x3])" is: 
--     (Forall 12 (Pred "R" [Var 12,Var 3]))
-- The first argument of this Forall is 12, which stands for 
-- the variable x12.  In parsing the quantified variable in 
-- such expressions, do something like 
--     (Var n) <- var
-- to parse the quantified variable and to bind the variable's
-- number to n.

silly = Pred "Foo" [Var 0]

-- EXISTSEXPR  ::=  Exists VAR (EXPR)
existsExpr  = fixMe

-- NEGEXPR     ::=  ~ATOM
negExpr    = fixMe

-- TERM        ::=  FACTOR { & FACTOR }^*
term       = fixMe

-- FACTOR      ::=  NEGEXPR | FORALLEXPR | EXISTSEXPR | ATOM
factor     = fixMe

-- EXPR        ::=  TERM   { + TERM }^*
expr :: ReadP Expr
expr       = fixMe

---------------------------------------------------------------------------
-- top level QuickCheck properties

prop1, prop2 :: Expr -> Bool

-- Try:   quickCheck prop1
prop1 e = (e == parseWith expr (showExpr e))

-- Try:   quickCheck prop2
prop2 e = (e == parseWith expr (showExprl e))
