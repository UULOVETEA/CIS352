import Parsing 
import AS
import DrawExpr
import Test.QuickCheck
import Data.Char
import Data.Time.Calendar
import qualified Data.Map.Strict as M

------------------------------------------------------------------------
pleaseFix = error "Please fix me."
------------------------------------------------------------------------
-- (test p ok bad) tests that p accepts the strings in ok and rejects
--    the strings in bad and, if things went wrong, reports the problems.

test :: Parser () -> [String] -> [String] -> IO ()
test p ok bad = do { tryA p ok; tryR p bad }
                   
tryA p ss = if null fails
            then putStr "Accept tests passed\n"
            else putStr ("Fail to accept:\n "++(unlines fails))
    where fails = [ s| s <- ss, parse p s /= [((),"")] ]

tryR p ss = if null fails
            then putStr "Reject tests passed\n"
            else putStr ("Fail to reject:\n "++(unlines fails))
    where fails = [ s| s <- ss, parse p s == [((),"")] ]

------------------------------------------------------------------------

------------------------------------------------------------------------
-- Sample parser 1 for the grammar
--     S ::= X# 
--     X ::= aXb | eps 
-- where eps stands for the empty string.
-- Note: L(S) = { a^n b^n # | n>=0 }

s1, x1 :: Parser ()
s1 = do { x1
        ; char '#'
        ; return ()
        }

x1 = do { char 'a'
        ; x1
        ; char 'b'
        ; return ()
        }
       +++
       return ()

ok1  = ["#","ab#","aabb#","aaabbb#","aaaabbbb#","aaaaabbbbb#"]
bad1 = ["a#","abb#","aab#","ba#","aabbb#"]

-- evaluate test1 to test parser s1
test1 = test s1 ok1 bad1 

------------------------------------------------------------------------
-- Sample parser 2 for the grammar
--     S ::= X# | #
--     X ::= aaXb | eps 
-- where eps stands for the empty string.
-- Note: L(S) = { a^(2n) b^n # | n>=0 }

s2, x2 :: Parser ()
s2 = do { x2            -- S2 ::=  X2 # | #
        ; char '#'
        ; return ()
        }
     +++
     do { char '#' ; return () }

x2 = do { char 'a'      -- X2 ::= a a X2 b | eps
        ; char 'a'
        ; x2
        ; char 'b'
        ; return ()
        }
       +++
       return ()

ok2  = ["#","aab#","aaaabb#","aaaaaabbb#","aaaaaaaabbbb#"]
bad2 = ["a#","abb#","abab#","ba#","aaaabbb#"]

-- evaluate test2 to test parser s3
test2 = test s2 ok2 bad2

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Homework Problem 4: Build a parser for the grammar
--   S ::=  (P) S  |  #
--   P ::=  (P) P  |  eps
-- where eps stands for the empty string.

s3, p3 :: Parser ()
s3 = do { char '('
        ; p3
        ; char ')'
        ; s3
        }
     +++
     do { char '#'; return () }

p3 = do { char '('
        ; p3
        ; char ')'
        ; p3
        }
     +++
     do { return () }

ok3  = ["#","()#","()()#","(()())#","()(())(()())#","((()())())#"]
bad3 = ["(#","())#",")(#","()(#","((())))#"]

-- evaluate test3 to test parser s3
test3 = test s3 ok3 bad3 

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Homework Problem 5 (Hutton Problem 6) -- I did the divide (/) case 
--    for you.  You have to fill in the minus (-) case. (EASY!)
------------------------------------------------------------------------
-- expr' is a parser for 
--    E ::= T + E | T
--    T ::= F * T | F / T | F
--    F ::= Num | (E)
-- it constructs a parse tree for the expression while parsing it.
-- Note that + and * are treated right-associatively.
expr', term', factor' :: Parser Exp
expr' = do { t <- term'
           ; plusE t    -- handle: +E
             +++ 
             minusE t
             +++
             return t   -- handle: none-of-the-above
           }
plusE t = do { symbol "+" ; e <- expr' ; return (Add t e) }
minusE t = do { symbol "-" ; e <- expr' ; return (Sub t e) }          

term' = do { f <- factor'
           ; timesT f   -- handle: *T
             +++
             divT f     -- handle: /T
             +++ 
             return f   -- handle: none-of-the-above
          }
timesT f = do { symbol "*"; t <- term' ; return (Mult f t) }
divT f   = do { symbol "/"; t <- term' ; return (Div  f t) }

factor' = do { symbol "(" ; e <- expr' ; symbol ")" ; return e }
          +++ 
          do { n <- natural ; return (Num n) }
    
-- When you think you have things working, try:
--   quickCheck prop_parse1 
prop_parse1 e = e==t
    where
      ((t,_):_) = parse expr' (showExprr e) 

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Homework Problem 6

leftExpr, leftTerm, leftFactor :: Parser Exp
leftExpr = do { tl <- leftTerm
              ; do { symbol "+"
                   ; e <- leftExpr
                   ; return (Add tl e)
                   }
                +++
                do { symbol "-"
                   ; e <- leftExpr
                   ; return (Sub tl e)
                   }
                +++ 
                return tl
              }

-- leftTerm is a parser for the left-associtive grammar
--   T ::= T * F | F
--   F ::= Num
-- E.g., parse leftTerm "1*2*3*4" produces
-- [(Mult (Mult (Mult (Num 1) (Num 2)) (Num 3)) (Num 4)),"")].
-- (Mult (Mult (Mult (Num 1) (Num 2)) (Num 3)) (Num 4)) has the parse tree:
--            *
--          /   \
--         *     4
--       /   \
--      *     3
--    /   \
--   1     2

leftTerm = do { tl <- leftFactor
              ; (fromLeft tl) ++ (fromLeft tl)
              }
    where
      fromLeft tl
          = timesF tl
            +++
            return tl
      timesF tl = do { symbol "*"; tr <- leftFactor; fromLeft (Mult tl tr) } +++
      do { symbol "/"; tr <- leftFactor; formLeft (Div tl tr) )

leftFactor = do { symbol "("
                ; e <- leftTerm -- when you define leftExpr, comment out 
                -- ; e <- leftExpr -- when you define leftExpr, comment in
                ; symbol ")"
                ; return e
                }
             +++
             do { n <- natural ; return (Num n) }
 
-- When you think you have things working, try:
--   quickCheck prop_parse2
prop_parse2 e = e==t
    where
      ((t,_):_) = parse leftExpr (showExprl e) 


-- The following may be useful for debugging while developing.
-- Right now, simplify changes all the Add's, Sub's, and Div's in an
-- Exp to Mult's.  So if your parser handles only Mult's and Num's,
-- prop_simp first simplifies the random expression e and tests the
-- parser leftTerm on the simplified expression.  As your parser
-- gets fancier (e.g., it can handle Num's, Mult's, and Div's), then
-- you can revise simplify (e.g., Div's don't get changed to
-- Mult's).

simplify (Sub e1 e2)  = Mult (simplify e1) (simplify e2)
simplify (Div e1 e2)  = Mult (simplify e1) (simplify e2)
-- simplify (Div e1 e2)  = Div (simplify e1) (simplify e2)
simplify (Mult e1 e2) = Mult (simplify e1) (simplify e2)
simplify (Add e1 e2)  = Mult (simplify e1) (simplify e2)
simplify (Num n)      = Num n

prop_simp e 
    = e'==t
      where
        e' = simplify e
        [(t,_)] = parse leftTerm (showExprl e') 


------------------------------------------------------------------------
-- Homework Problem 7

-- Sample dates (having your code get these right is enough testing 
-- for this problem.
date1, date2, date3 :: String
date1 = "Tue Feb 29 03:18:00 GMT 2000"
date2 = "Fri Apr  7 09:49:09 EDT 2017"
date3 = "Thu May 4 14:40:00 EDT 2017"

after100 :: Parser Day
after100 = pleaseFix 

-- some gifts ---

-- (timeChar c) returns True iff c is a digit or a colon.
timeChar :: Char -> Bool
timeChar c = isDigit c || c==':'
                
data Month = Jan | Feb | Mar | Apr | May | Jun 
           | Jul | Aug | Sep | Oct | Nov | Dec
             deriving (Show,Read,Eq,Enum)

-- (newDay yr mon day) = the specified Day
-- Ex: (newDay 2016 "May" 1) returns the Day 2016-05-01.
newDay :: Int -> String -> Int -> Day
newDay yr mon day 
    = fromGregorian (fromIntegral yr) (1+(fromEnum (read mon :: Month))) day

