-- Written by: Xiaomeng Cao

import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT (TFun t t') 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv 
  = TypeTable    -- i.e. [(String, Type)]

type Sub 
  = TypeTable    -- i.e. [(String, Type)]  

-- Built-in function types...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

------------------------------------------------------
-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key table
  = fromJust (lookup key table)

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp key def table
  = fromMaybe def (lookup key table)

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp key table
  = [value | (value,key') <- table, key' == key]

occurs :: String -> Type -> Bool
occurs s (TFun t1 t2) = occurs s t1 || occurs s t2
occurs s (TVar s')    = s==s'
occurs _ _            = False

------------------------------------------------------
-- PART II

-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All type variables in the expression have a binding in the given 
--      type environment
inferType :: Expr -> TEnv -> Type
inferType (Number _) env 
  = TInt
inferType (Boolean _) env 
  = TBool
inferType (Id x) env 
  = lookUp x env
inferType (Prim x) env 
  = lookUp x primTypes
inferType (Cond e1 e2 e3) env 
  |(inType e1 == TBool) && ((inType e2) == (inType e3)) = inType e2 
  |otherwise = TErr 
  where  
    inType a = inferType a env
inferType (App e1 e2) env 
  = inferApp e1 e2
  where  
    inType a = inferType a env
    inferApp f a
      |inType f == (TFun t t') && (inType a == t) = t'
      |otherwise = TErr  
      where 
      (TFun t t') = inType f 

------------------------------------------------------
-- PART III

applySub :: Sub -> Type -> Type
applySub s (TFun t t') 
  = TFun (applySub s t) (applySub s t') 
applySub s v@(TVar a)
  = tryToLookUp a v s
applySub _ a = a

unify :: Type -> Type -> Maybe Sub
unify t t'
  = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] s 
  = (Just s)
unifyPairs ((t, (TVar v)):ps) s 
  = unifyPairs (((TVar v), t):ps) s
unifyPairs (((TVar v), t):ps) s
  |occurs v t = Nothing
  |(TVar v) == t = unifyPairs ps s
  |otherwise = unifyPairs (map (\(c,d) -> ((applySub s' c),(applySub s' d))) ps) s'
  where 
    s' = ((v, t):s)
unifyPairs (((TFun t1 t2), (TFun t1' t2')):ps) s
  = unifyPairs ((t1,t1'):(t2,t2'):ps) s 
unifyPairs ((a,b):ps) s
  |a == b = unifyPairs ps s 
  |otherwise = Nothing 

------------------------------------------------------
-- PART IV

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub
  = map modify tenv
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs 
  = foldr1 combine

inferPolyType :: Expr -> Type
inferPolyType exp
  = resType
  where 
    (_, resType, _) = inferPolyType' exp [] infNList 
    infNList        = [concat ["a", show n] | n <- [1..]]

-- You may optionally wish to use one of the following helper function declarations
-- as suggested in the specification. 

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
inferPolyType' (Number _) _ s
  = ([], TInt, s)
inferPolyType' (Boolean _) _ s
  = ([], TBool, s)
inferPolyType' (Id x) env s
  = ([], lookUp x env, s)
inferPolyType' (Prim x) env s
  = ([], lookUp x primTypes, s)
inferPolyType' (Fun x e) env (s:ss)
  = (sub, res, ss')
  where
    (sub, te, ss')  = inferPolyType' e ((x, TVar s):env) ss
    sth = applySub sub (TVar s) 
    res = if te == TErr then TErr else TFun sth te
inferPolyType' (App f e) env (s:ss)
  | isJust resSub 
    = ((combineSubs [fromJust resSub, sub', sub]), applySub (fromJust resSub) (TVar s), ss'')
  | otherwise = ([], TErr, [])
  where
    (sub, tf, ss') = inferPolyType' f env ss
    (sub', te, ss'') = inferPolyType' e (updateTEnv env sub) ss'
    resSub = unify tf (TFun te (TVar s)) 

-- inferPolyType' :: Expr -> TEnv -> Int -> (Sub, Type, Int)
-- inferPolyType' 
--   = undefined

------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))

------------------------------------------------------------------------
-- Add this to the end of the template file

-- Part I Tests: Evaluate ti and see if you get the expected result
t1  = lookUp "x" env -- Expected result: TInt
t2  = lookUp "c" env -- Expected result: TBool
t3  = lookUp "z" env -- Expected result: an error

t4  = tryToLookUp "x" TErr env -- Expected result: TInt
t5  = tryToLookUp "c" TErr env -- Expected result: TBool
t6  = tryToLookUp "z" TErr env -- Expected result: TErr

t7  = reverseLookUp 1 [("hello",1),("x",9),("dolly",1)]
     -- Expected result: ["hello","dolly"]
t8  = reverseLookUp (TFun TInt (TFun TInt TBool)) primTypes
     -- Expected result: [">","=="]
t9  = reverseLookUp TErr primTypes
     -- Expected result: []     

t10 = occurs "x" (TVar "y")              -- Expected result: False
t11 = occurs "x" TBool                   -- Expected result: False
t12 = occurs "x" (TFun TBool (TVar "x")) -- Expected result: True

-- Part II Tests: Evaluate testIT, if successful, nothing is printed;
--   if not successful, you will get info about what tests failed.

makeTestIT exp ty = do let result = inferType exp env
                       if (result/=ty)
                         then putStrLn fuss
                         else return ()
    where fuss = ("(inferType ("++(show exp) ++ ") env) /= "
                 ++ (show ty)  ++ ", the expected value")

-- If successful, nothing is printed or returned.
testIT = sequence_ [ makeTestIT ex1 type1
                   , makeTestIT ex2 type2
                   , makeTestIT ex3 type3
                   , makeTestIT ex4 type4
                   , makeTestIT ex5 type5
                   , makeTestIT ex6 type6
                   , makeTestIT ex7 type7
                   , makeTestIT ex8 type8
                   ]

-- Part III Tests
--   For t13, t14, and t15, evaluate the expression and see you get the
--     expected result.
--   Evaluate testIT, if successful, nothing is printed;
--     if not successful, you will get info about what tests failed

test_sub = [("a",TBool),("b",TFun TBool TInt)]
t13 = applySub test_sub TInt -- Expected result: TInt          
t14 = applySub test_sub (TFun TBool (TVar "a"))
                             -- Expected result: TFunc TBool TBool
t15 = applySub test_sub (TVar "c")
                             -- Expected result: TVar "c"      

makeTestU ua ub sub = do if (unify ua ub) /= sub
                           then putStrLn fuss
                           else return ()
    where fuss = ("(unify "++(show ua) ++ " " ++ (show ub)
                 ++ ") /= " ++ (show sub) ++ ", the expected value")

-- If successful, nothing is printed or returned.
testU = sequence_ [ makeTestU u1a u1b sub1
                  , makeTestU u2a u2b sub2
                  , makeTestU u3a u3b sub3
                  , makeTestU u4a u4b sub4
                  , makeTestU u5a u5b sub5
                  , makeTestU u6a u6b sub6
                  ]
------------------------------------------------------------------------      
-- Part IV Tests

-- Use ex9, ..., ex14 and type9, ..., type14 defined above for tests.

