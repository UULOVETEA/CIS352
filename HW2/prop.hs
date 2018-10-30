------------------------------------------------------------------------
-- Written by: Xiaomeng Cao
------------------------------------------------------------------------
-- Homework 3, Part I
-- Propositional logic, based on an example of W. Heijltjes & P. Wadler

import Control.Monad( liftM, liftM2 )
import Data.List( nub, sort )
import Test.HUnit
import Test.QuickCheck( quickCheck, sample,
                        Arbitrary( arbitrary ), Gen,
                        oneof, elements, sized  )

pleaseFix funName = error (funName ++ ": Please fix me.")



-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          | Prop :#: Prop   -- I did problem 1(a) for you.
          deriving (Eq, Ord)

-- some samples
vp, vq, vr, p1, p2, p3 :: Prop
vp = Var "P"
vq = Var "Q"
vr = Var "R"
p1 = ((vp :|: vq) :&: (vp :&: vq))
p2 = ((vp :|: vq) :&: ((Not vp) :&: (Not vq)))
p3 = ((vp :&: (vq :|: vr))
      :&: (((Not vp) :|: (Not vq)) :&: ((Not vp) :|: (Not vr))))

type Names = [Name]
type Env = [(Name, Bool)]

lookUp :: Name -> Env -> Bool
lookUp x env = case (lookup x env) of
                 (Just b) -> b
                 Nothing  -> False

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)     =  x
showProp (F)         =  "F"
showProp (T)         =  "T"
showProp (Not p)     =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)   =  "("  ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)   =  "("  ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)  =  "("  ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q) =  "("  ++ showProp p ++ "<->" ++ showProp q ++ ")"
showProp (p :#: q)   =  "("  ++ showProp p ++ "#" ++ showProp q ++ ")"

-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)     =  lookUp x e
eval e (F)         =  False
eval e (T)         =  True
eval e (Not p)     =  not (eval e p)
eval e (p :|: q)   =  eval e p || eval e q
eval e (p :&: q)   =  eval e p && eval e q
eval e (p :->: q)  =  (not (eval e p)) || eval e q
eval e (p :<->: q) =  eval e p == eval e q
eval e (p :#: q)   =  not (eval e p && eval e q)

------------------------------------------------------------------------

-- names p = a list of all the names in p (with no reps)
names :: Prop -> Names
names (Var x)       =  [x]
names (F)           =  []
names (T)           =  []
names (Not p)       =  names p
names (p :|: q)     =  nub (names p ++ names q)
names (p :&: q)     =  nub (names p ++ names q)
names (p :->: q)    =  nub (names p ++ names q)
names (p :<->: q)   =  nub (names p ++ names q)
names (p :#: q)     =  nub (names p ++ names q)

-- envs [n1,...,nk] = a list of all possible environments for these names.
-- E.g.: 
envs :: Names -> [Env]
envs []     = [[]] 
envs [x]    = [[(x,True)],[(x,False)]]
envs (x:xs) = [(x,True):t | t <- ts] ++ [(x,False):t | t <- ts]
    where
      ts = envs xs

satisfiable :: Prop -> Bool
satisfiable p = or [eval e p| e <- envs(names p)]

tautology :: Prop -> Bool
tautology p =  and [eval e p| e <- envs(names p)]

equivalent, equivalent' :: Prop -> Prop -> Bool
equivalent p q = and [eval e p == eval e q | e <- envs theNames]
    where theNames = nub (names p ++ names q)

equivalent' p q = tautology (p :<->: q)

equiv1_prop :: Prop -> Prop -> Bool
equiv1_prop p q = (equivalent p q == equivalent' p q)

equiv2_prop :: Prop -> Bool
equiv2_prop p = (equivalent p p) && (equivalent' p p)

subformulas :: Prop -> [Prop]
subformulas (Var x)     =  [Var x]
subformulas (F)         =  [F]
subformulas (T)         =  [T]
subformulas (Not p)     =  (Not p):subformulas p
subformulas (p :|: q)   =  (p :|: q):nub (subformulas p ++ subformulas q)
subformulas (p :&: q)   =  (p :&: q):nub (subformulas p ++ subformulas q)
subformulas (p :->: q)  =  (p :->: q):nub (subformulas p ++ subformulas q)
subformulas (p :<->: q) =  (p :<->: q):nub (subformulas p ++ subformulas q)
subformulas (p :#: q)   =  (p :#: q):nub (subformulas p ++ subformulas q)


------------------------------------------------------------------------
-- Problem 1 -----------------------------------------------------------
-- For this, modify the definitions of Prop, showProp, eval, names,
-- and subformulas as directed and then run (fullTable (p :#: q)).
 
------------------------------------------------------------------------
-- Problem 2 -----------------------------------------------------------
nandify :: Prop -> Prop
nandify (Var x)     =  Var x
nandify (F)         =  F
nandify (T)         =  T
nandify (Not p)     =  nandify (p :#: p)
--nandify (p :|: q)   =  pleaseFix "nandify"
--nandify (p :&: q)   =  pleaseFix "nandify"
nandify (p :->: q)  =  nandify (p :->: q)
nandify (p :<->: q) =  nandify ((p :->: q) :&: (q :->: p)) -- a gift
--nandify (p :#: q)   =  pleaseFix "nandify"

-- (justNands p) checks that the only constructor used in p is :#:
justNands (Var x)   = True
justNands (F)       = True
justNands (T)       = True
justNands (p :#: q) = (justNands p) && (justNands q)
justNands _         = False


-- HUnit tests for toNNF
-- For information on HUnit, see: http://hackage.haskell.org/package/HUnit
testNand1 = TestCase (assertEqual
                      "nandify (Not vp),"
                      (vp :#: vp)
                      (nandify (Not vp)))
testNand2 = TestCase (assertEqual
                      "nandify (vp :&: (Not vq)),"
                      ((vp :#: (vq :#: vq)):#:(vp :#: (vq :#: vq)))
                      (nandify (vp :&: (Not vq))))
testNand3 = TestCase (assertEqual
                      "nandify (vp :<->: vq),"
                      (((vp:#:(vq:#:vq)):#:(vq:#:(vp:#:vp)))
                       :#:
                       ((vp:#:(vq:#:vq)):#:(vq:#:(vp:#:vp))))
                      (nandify (vp :<->: vq)))
testsNand = TestList [ TestLabel "testNand1" testNand1
                     , TestLabel "testNand2" testNand2
                     , TestLabel "testNand3" testNand3
                     ]

-- You can run these tests by evaluating:
--    runTestTT testsNand           
-- You can run just testNand3 by evaluating:
--    runTestTT testNand3           

------------------------------------------------------------------------
-- Problem 3 -----------------------------------------------------------
isNNF :: Prop -> Bool
isNNF (p :&: q)      =  isNNF p && isNNF q
isNNF (p :|: q)      =  isNNF p && isNNF q
isNNF (Not (Var _))  =  True
isNNF (Var _)        =  True
isNNF T              =  True
isNNF F              =  True
isNNF _              =  False

------------------------------------------------------------------------
-- Problem 4 -----------------------------------------------------------
toNNF :: Prop -> Prop
toNNF (p :|: q)       = toNNF p :|: toNNF q
toNNF (p :&: q)       = toNNF p :&: toNNF q
toNNF (p :->: q)      = toNNFnot p :|: toNNF q
toNNF (p :<->: q)     = (toNNFnot p :|: toNNF q) :&: (toNNF p :|: toNNFnot q)
toNNF (Not p)         = toNNFnot p
toNNF p               = p

toNNFnot (p :|: q)    = toNNFnot p :&: toNNFnot q
toNNFnot (p :&: q)    = toNNFnot p :|: toNNFnot q
toNNFnot (p :->: q)   = toNNF p :&: toNNFnot q
toNNFnot (p :<->: q)  = (toNNFnot p :|: toNNFnot q) :&: (toNNF p :|: toNNF q)
toNNFnot (Not p)      = toNNF p
toNNFnot T            = F
toNNFnot F            = T
toNNFnot (Var a)      = Not (Var a)

-- HUnit tests for toNNF
testNNF1 = TestCase (assertEqual
                     "toNFF (Not T),"
                     F
                     (toNNF (Not T))
                    )
testNNF2 = TestCase (assertEqual
                     "toNNF (Not F),"
                     T
                     (toNNF (Not F))
                    )
testNNF3 = TestCase (assertEqual
                     "toNNF (Not (Not (Not vp))),"
                     (Not vp)
                     (toNNF (Not (Not (Not vp))))
                    )
testNNF4 = TestCase (assertEqual
                     "toNNF (Not (Not (Not (Not vp)))),"
                     vp
                     (toNNF (Not (Not (Not (Not vp)))))
                    )
testsNNF = TestList [ TestLabel "testNNF1" testNNF1
                    , TestLabel "testNNF2" testNNF2
                    , TestLabel "testNNF3" testNNF3
                    , TestLabel "testNNF4" testNNF4
                    ]

-- You can run these tests by evaluating:
--    runTestTT testsNNF
-- To run just testNNF5, evaluate:
--    runTestTT testNNF5
                          
--------------------------------------------------------------------------
-- The following is lifted from the Univ. of Edinburg --------------------
--------------------------------------------------------------------------


-- For QuickCheck --------------------------------------------------------

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)

{- To see what kind of propositions are generated, try:
   sample (arbitrary :: Gen Prop)
-}
------------------------------------------------------------------------
-- QuickCheck tests

-- check if the result of nandify contains just nands.
nand1_prop :: Prop -> Bool
nand1_prop p = justNands(nandify p)

-- check if (toNNF p) is equivalent to p
nand2_prop :: Prop -> Bool
nand2_prop p = equivalent p (nandify p)

-- check if result of toNNF is in neg. normal form
nnf1_prop :: Prop -> Bool
nnf1_prop p = isNNF (toNNF p)

-- check if (toNNF p) is equivalent to p
nnf2_prop :: Prop -> Bool
nnf2_prop p = equivalent p (toNNF p)

-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
    where
      n = length s
      h = (w - n) `div` 2
              
-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True

