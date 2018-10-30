-- version 1.2, 15 Feb 2018
module State where
import qualified Data.Map.Strict as M
import Data.List (intercalate)

------------------------------------------------------------------------
-- Contents
--   * Types for locations and states
--   * empty: an empty state
--   * basic operations on states: fetch, set, load, dump
--   * prnState: pretty printer for  states
--   * state4: a state with state4(0)=4 state4(1)=...=state4(4)=0.
--   * texState, texState', and texState'': ignore these
------------------------------------------------------------------------

------------------------------------------------------------------------
-- modeling state

type Location = Int 
type State = M.Map Location Int

-- empty = an empty state
empty :: State
empty = M.empty

-- fetch s i = s(i), if i is in dom(s), o/w BOOM!
fetch :: State -> Location -> Int
fetch s i = s M.! i
-- fetch s i = M.findWithDefault 0 i s  -- Alt: o/w 0

-- set s i v = s[i |-> v]
set :: State -> Location -> Int -> State
set s i v = M.insert i v s

-- load s [x0,x1,...,xk] = the modification of s with s(i)=xi, i=0,...,k.
load :: State -> [Int] -> State
load s vs = M.fromList $ zip [0..] vs

-- dump s = [(i,s(i)) : i is in dom(s)]
dump :: State -> [(Location,Int)]
dump s = M.toList s

------------------------------------------------------------------------
-- Prints a state
-- Try: prnState state4

prnState :: State -> IO ()
prnState s 
    = do { let ps = [ (i,v) | (i,v) <- dump s, v /= 0]
         ; putStr "Locs(!=0): "
         ; mapM_ putStr ["X["++show i++"]="++show (x)++" " | (i,x) <- ps]
         ; putStrLn ""
         }

------------------------------------------------------------------------
-- Various ways of translating a state to a TeX expression.  E.g.:
-- (texState state4)   ~~> "s_{4,0,0}"
-- (texState' state4)  ~~> "s_{4}"
-- (texState'' state4) ~~> "\\ST{[\\ell_{0}\\mapsto 4]}"

texState :: State -> String
texState s =  "s_{" ++ intercalate "," (map show subs) ++ "}"
    where subs = map snd $ take (n+1) $ dump s
          n    = 2

texState' :: State -> String
texState' s =  "s_{" ++ intercalate "," (map show subs) ++ "}"
    where subs = foldr step []  $ map snd $ dump s
          step x r = if (null r)&&(x==0) then [] else x:r


texState'' :: State -> String
texState'' s =  (concatMap pack ps) ++"]}"
    where 
      ps = zip [ (i,v) | (i,v) <- dump s, v /= 0]
                        ("\\ST{[\\ell_{":repeat ",\\, \\ell_{");
      pack ((i,x),pre) = pre++show i++"}\\mapsto "++ show x
                         
------------------------------------------------------------------------

-- a state with state4(0)=4 state4(1)=...=state4(4)=0.
state4 = load empty [4,0,0,0,0]
--        locations: 0 1 2 3 4

