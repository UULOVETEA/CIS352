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

