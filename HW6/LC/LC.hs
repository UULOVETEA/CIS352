module LC where
-- version 1.4, 23 Feb 2018
import State (Location)
-- import Data.Word (Word8)
import Test.QuickCheck
import Control.Monad

------------------------------------------------------------------------------
-- Contents:
--   * Types for the abstract syntax for LC
--     E.g., AExp, AOp, BExp, COp, and Command
--   * Little utilities (aApply and bApply) for
--     applying operators (e.g., Plus and Eq) to
--     numeric arguments
--   * Pretty (but not too pretty) printing of the LC
--     abstract syntax
--   * QuickCheck test generators
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Abstract syntax

-- type Location = Int  -- defined in the State module

data AExp    = AConst Int | Val Location | ABin AOp AExp AExp
               deriving (Eq)

data AOp     = Plus | Minus | Times
               deriving (Eq,Ord,Enum)

data BExp    = BConst Bool | Compare COp AExp AExp
               deriving (Eq)

data COp     = Eq | Neq | Lt | Leq | Gt | Geq
               deriving (Eq,Ord,Enum)

data Command = Skip | Set Location AExp | Seq Command Command
             | If BExp Command Command | While BExp Command
             | DoWhilst Command BExp
               deriving (Eq)

------------------------------------------------------------------------
-- Utilities

-- aApply iop n1 n2 = (n1 iop n2)
-- E.g., (aApply Plus 3 4) ~~> 7
aApply :: AOp -> Int -> Int -> Int
aApply Plus  n1 n2 = n1+n2
aApply Minus n1 n2 = n1-n2
aApply Times n1 n2 = n1*n2

-- bApply cop n1 n2 = (n1 cop n2)
-- E.g.:  (bApply Eq 3 4) ~~> False
-- E.g.:  (bApply Eq 3 3) ~~> True
bApply :: COp -> Int -> Int -> Bool
bApply Eq  v1 v2 = (v1==v2)
bApply Neq v1 v2 = (v1/=v2)
bApply Lt  v1 v2 = (v1<v2)
bApply Leq v1 v2 = (v1<=v2)
bApply Gt  v1 v2 = (v1>v2)
bApply Geq v1 v2 = (v1>=v2)

------------------------------------------------------------------------
-- Examples

-- a sample integer expression: ((val(x0)*3)+7)
exA = ABin Plus (ABin Times (Val 0) (AConst 3)) (AConst 7)

-- a sample boolean expression: (((val(x0)*3)+7)>5)
exB = Compare Gt exA (AConst 5)

-- a command that computes factorial of val(x0)
exC = Seq (Set 1 (AConst 1))
          (Seq (Set 2 (Val 0)) 
               (While (Compare Gt (Val 2) (AConst 0)) 
                      (Seq (Set 1 (ABin Times (Val 1) (Val 2)))
                           (Set 2 (ABin Minus (Val 2) (AConst 1))))))

-- { x1 := 1; 
--   x2 := val(x0); 
--   while (val(x2)>0) do { 
--     x1 := (val(x1)*val(x2));  
--     x2 := (val(x2)-1) 
--   }
-- }

------------------------------------------------------------------------
-- Display


instance Show Command where
    show Skip = "skip"
    show (Set loc ae) = "x" ++ show loc ++ " := " ++ show ae
    show (Seq cmd1 cmd2) = "{ "++ show cmd1 ++ showCmds cmd2 ++ " }"
        where showCmds (Seq c1 c2) = "; "++show c1++showCmds c2
              showCmds c           = "; "++show c
    show (If be cmd1 cmd2) 
        = "if " ++ show be ++ " then " ++ show cmd1 ++ " else { "
          ++ show cmd2 ++ " }"
    show (While be cmd) 
        = "while " ++ show be ++ " do " ++ show cmd
    show (DoWhilst cmd be) 
        = " do " ++ show cmd ++ " whilst " ++ show be 

instance Show BExp where
    show (BConst b) = if b then "tt" else "ff"
    show (Compare cop ae1 ae2) = "("++show ae1++show cop++show ae2++")"

instance Show AExp where
    show (AConst n) = if n<0 then "("++show n++")" else show n
    show (Val loc)  = "val(x" ++ show loc ++ ")"
    show (ABin iop ie1 ie2) = "("++show ie1++show iop++show ie2++")"

instance Show COp where
    show Eq = "=="
    show Neq = "/="
    show Lt  = "<"
    show Leq = "<="
    show Gt  = ">"
    show Geq = ">="

instance Show AOp where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"

------------------------------------------------------------------------
-- QuickCheck test generators
------------------------------------------------------------------------

instance Arbitrary AOp where
    arbitrary = elements [Plus,Minus,Times]

instance Arbitrary COp where
    arbitrary = elements [Eq,Neq,Lt,Leq,Gt,Geq]    

instance Arbitrary AExp where
    arbitrary = sized exp
        where genNum = liftM AConst arbitrary
              genVal = elements [ Val loc | loc <- [0..4]]
              exp 0 = oneof [ genNum, genVal ]
              exp n = oneof [ genNum
                            , genVal
                            , liftM3 ABin arbitrary subexp subexp 
                            ]
                        where subexp = exp (n `div` 2)

instance Arbitrary BExp where
    arbitrary = oneof [ genTF
                      , liftM3 Compare arbitrary  arbitrary  arbitrary 
                      ]
        where genTF  = elements [BConst False, BConst True]

-- For testing stuff that executes programs, we do **NOT** generate
-- while-commands since they may could be infinite loops.
instance Arbitrary Command where
--    arbitrary = liftM2 Seq genPre (sized stm)
  arbitrary = sized cmd
        where genSet = liftM2 Set loc arbitrary
                  where loc = elements [0..4]
              cmd 0 = oneof [return Skip, genSet]
              cmd n = frequency [ (1,return Skip)
                                , (3, genSet)
                                , (3, liftM2 Seq subcmd subcmd)
                                , (2, liftM3 If arbitrary subcmd subcmd)
                                -- , (1, liftM2 While arbitrary subcmd)
                                -- , (1, liftM2 DoWhilst subcmd arbitrary)
                                ]
                  where subcmd = cmd (n `div` 2)

------------------------------------------------------------------------                                 
