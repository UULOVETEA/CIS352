-- Functional parsing library from chapter 8 of Programming in
-- Haskell, Graham Hutton, Cambridge University Press, 2007.
-- 
-- Modified 6 April 2015 by Jim Royer to (correctly??) add Parser as
-- an instance of the Functor, Applicative, ... type classes so that
-- GHC 8.0 will still like the code.
 
module Parsing where

import Data.Char
import Data.Functor
import qualified Control.Applicative as App (Applicative(..),Alternative(..))
import Control.Monad

infixr 5 +++ 

-- The Parser type
------------------

newtype Parser a  =  P (String -> [(a,String)])

-- Joining up Parser to the Function, Applicative, Alternative, and
-- Monad, and MonadPlus type classes.

instance Functor Parser where
    fmap f p      =  P (\inp -> [ (f v,out) | (v,out) <- parse p inp])

instance App.Applicative Parser where
    pure          =  return
    (<*>)         =  ap 

instance App.Alternative Parser where
    empty         =  mzero
    (<|>)         =  mplus

instance Monad Parser where
   return v       =  P (\inp -> [(v,inp)])
   p >>= f        =  P (\inp -> case parse p inp of
                                  []        -> []
                                  [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
   mzero          =  P (\inp -> [])
   p `mplus` q    =  P (\inp -> case parse p inp of
                                  []        -> parse q inp
                                  [(v,out)] -> [(v,out)])


-- Basic parsers
----------------

failure           :: Parser a
failure           =  mzero

item              :: Parser Char
item              =  P (\inp -> case inp of
                                  []     -> []
                                  (x:xs) -> [(x,xs)])

parse             :: Parser a -> String -> [(a,String)]
parse (P p) inp   =  p inp

-- Choice
---------

(+++)             :: Parser a -> Parser a -> Parser a
p +++ q           =  p `mplus` q

-- Derived primitives
---------------------

sat               :: (Char -> Bool) -> Parser Char
sat p             =  do x <- item
                        if p x then return x else failure

digit, lower, upper, letter, alphanum  :: Parser Char

digit             =  sat isDigit
lower             =  sat isLower
upper             =  sat isUpper
letter            =  sat isAlpha
alphanum          =  sat isAlphaNum

char              :: Char -> Parser Char
char x            =  sat (== x)

string            :: String -> Parser String
string []         =  return []
string (x:xs)     =  do char x
                        string xs
                        return (x:xs)

many              :: Parser a -> Parser [a]
many p            =  many1 p +++ return []

many1             :: Parser a -> Parser [a]
many1 p           =  do v  <- p
                        vs <- many p
                        return (v:vs)

-- parse variable names
ident             :: Parser String
ident             =  do x  <- lower
                        xs <- many alphanum
                        return (x:xs)

-- parse natural numbers
nat               :: Parser Int
nat               =  do xs <- many1 digit
                        return (read xs)

-- parse integers (including negative ones)
int               :: Parser Int
int               =  do char '-'
                        n <- nat
                        return (-n)
                      +++ nat

-- eat spaces
space             :: Parser ()
space             =  do many (sat isSpace)
                        return ()

-- Ignoring spacing
-------------------

token             :: Parser a -> Parser a
token p           =  do space
                        v <- p
                        space
                        return v

identifier        :: Parser String
identifier        =  token ident

natural           :: Parser Int
natural           =  token nat

integer           :: Parser Int
integer           =  token int

symbol            :: String -> Parser String
symbol xs         =  token (string xs)

