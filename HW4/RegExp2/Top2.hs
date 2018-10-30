-------------------------------------------------------------------------- 
--									--
--	Top2.hs								--
--									--
--	Top level file in the NFA library.				--
--									--
--	(c) Simon Thompson, 1995, 2000					--
--	as munged by Jim Royer, 2016				--
--									--
-------------------------------------------------------------------------- 

module Top2 where

import BuildNfa2
import ImplementNfa
import NfaLib
import NfaToDfa
import NfaMisc
import NfaTypes
import RegExp2
import Matches2
import DrawNfa
import Data.List (inits,(\\))

-- Problem 5 (prefixes)

prefixes Epsilon     = Epsilon
prefixes (Literal c) = Or (Literal c) Epsilon  -- Bug fix!
prefixes (Or s t)    = Or (prefixes s) (prefixes t)
prefixes (Then s t)  = preErr (Then s t)
prefixes (Star s)    = preErr (Star s)
prefixes (Opt s)     = preErr (Opt s)
prefixes (Plus s)    = preErr (Plus s)


preErr s = error ("prefixes for "++(printRE s)++" not yet implemented!")

example = Then (Literal 'a') (Then (Star (Literal 'b')) (Literal 'c'))
preExample = prefixes example

-- Example elements of the language (prefixes example)
positives = take 20 (concat [['a':bs,'a':bs++['c']] | bs <-inits ['b','b'..]])

-- Example nonelements of the language (prefixes example)
negatives = (concatMap len [0..10]) \\ positives
  where
    len 0 = [""]
    len n = [c:xs |  xs <- len (n-1),c <- ['a','b','c']]

-- Both of the following should return True (if prefixes is correct)
tstPos = and [matches preExample s | s <- positives]
tstNeg = null [ s | s <- negatives, matches preExample s]
