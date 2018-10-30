import Parsing
import Data.Char
import DrawExpr
import AS
-- a^nb^n#

-- parser for L = { a^nb^n | n >= 0 }
anbn = do { char 'a'
          ; anbn
          ; char 'b'
          ; return ()
          }
       +++ return ()

-- another parser for L = { a^nb^n | n >= 0 }
anbn' = do { as <- many (char 'a')
           ; bs <- many (char 'b')
           ; char '#'
           ; if (length as == length bs) then return () else failure
           }

-- parser for L = { a^nb^n# | n >= 0 }
anbn'' = do { anbn ; char '#' ; return () }

               

-- Examples from Graham Hutton's slides
---------------------------------------

-- From page 12
p :: Parser (Char,Char)
p = do { x <- item
       ; item
       ; y <- item
       ; return (x,y)
       }

-- From page 18
q :: Parser String
q = do { char '['
       ; d <- digit
       ; ds <- many (do { char ','; digit })
       ; char ']'
       ; return (d:ds)
       }

-- word list parser  WL ::= () | (W{,W}^*)  where W ::= L^+   L ::= a letter
word = many1 letter

wordLst = do { char '('
             ; first <- word
             ; rest  <- many commaWord
             ; char ')'
             ; return (first:rest)
             }
commaWord = do { char ','
               ; nm <- word
               ; return nm
               }

wordLst' = do { string "()"; return [] } +++ wordLst

------------------------------------------------------------------------
-- Hutton's Arithmetic Expressions

-- expr   ::= term '+' expr | term         ::= term ('+' expr | epsilon)
-- term   ::= factor '*' term | factor     ::= factor ('*' term | epsilon)
-- factor ::= digit | '(' expr ')'
-- digit  ::= '0' | '1' | ... | '9'
--   Notes: + and * are right-assoc
--          prec(*) > prec(+)

-- expr parses and evaluates an arithmetic expression
expr :: Parser Int
expr = do { t <- term
          ; do { char '+'
               ; e <- expr
               ; return (t+e)
               }
            +++ return t
          }
term :: Parser Int
term = do f <- factor
          do char '*'
             t <- term
             return (f * t)   --- Why indenting and monads are tricky:
           +++ return f       --- Try removing a space in front of +++

factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
         +++ do char '('
                e <- expr
                char ')'
                return e

eval :: String -> Int
eval xs = fst (head (parse expr xs))
                   


------------------------------------------------------------------------
-- Instead of evaluating expressions, lets build the parse tree

-- expr' parses an arithmetic expression and returns its parse tree
-- See AS.hs for the abstract syntax.

expr' :: Parser Exp
expr' = do { t <- term'
           ; do { char '+'
                ; e <- expr'
                ; return (Add t e)
                }
           +++ return t
          }

term' :: Parser Exp
term' = do { f <- factor'
           ; do { char '*'
                ; t <- term'
                ; return (Mult f  t)
                }
             +++ return f
           }

factor' :: Parser Exp
factor' = do { d <- digit
             ; return (Num (digitToInt d))
               +++ do { char '('
                      ; e <- expr'
                      ; char ')'
                      ; return e
                      }
             }

eval' :: String -> Exp
eval' xs = fst (head (parse expr' xs))
                   
drawParse xs = drawExpr (eval' xs)

------------------------------------------------------------------------
-- A tokenized digit list
-- Try:  parse q      "[2, 3]"
-- Try:  parse dlist2 "[2, 3]"

dlist2 = do token(char '[')
            d <- token(digit)
            ds <- many (do {token(char ','); token(digit)})
            token(char ']')
            return (d:ds)

               
