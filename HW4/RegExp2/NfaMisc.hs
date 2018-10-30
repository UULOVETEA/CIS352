
-------------------------------------------------------------------------- 
--                                                                      --
--      NfaMisc.hs                                                      --
--                                                                      --
--      Misecllaneous definitions for the NFA system. Includes          --
--      examples of machines, and functions to print an NFA and the     --
--      equivalence classes produces by minimisation.                   --
--                                                                      --
--      Regular expressions are defined in RegExp, and the type of      --
--      NFAs in NfaTypes. The implementation of sets used is in         --
--      Sets.                                                           --
--                                                                      --
--      (c) Simon Thompson, 1995, 2000                                  --
--      munged by Jim Royer 2012                                        --
--                                                                      --
-------------------------------------------------------------------------- 

module NfaMisc where

import RegExp2
import Data.Set (Set)
import qualified Data.Set as S
import NfaTypes 

-------------------------------------------------------------------------- 
--                                                                      --
--      Examples                                                        --
--                                                                      --
-------------------------------------------------------------------------- 

machM, machN :: Nfa Int

machM     = NFA
            (S.fromList [0..3])
            (S.fromList [ Move 0 'a' 0 ,
                       Move 0 'a' 1,
                       Move 0 'b' 0,
                       Move 1 'b' 2,
                       Move 2 'b' 3 ] )
            0
            (S.singleton 3)

machN    = NFA
            (S.fromList [0..5])
            (S.fromList [ Move 0 'a' 1 ,
                       Move 1 'b' 2,
                       Move 0 'a' 3,
                       Move 3 'b' 4,
                       Emove 3 4,
                       Move 4 'b' 5 ] )
            0
            (S.fromList [2,5])

machM1   = NFA
            (S.fromList [0..2])
            (S.fromList [ Move 0 'a' 1 ,
                          Move 0 'b' 0,
                          Move 1 'a' 1,
                          Move 1 'b' 2,
                          Move 2 'a' 1,
                          Move 2 'b' 0] )
            0
            (S.fromList [2])



machx  = NFA
         (S.fromList [1..3])
            (S.fromList [ Emove 1 2
                        , Move 2 'a' 1
                        , Move 2 'a' 3
                        , Move 1 'b' 3
                        ])
            1
            (S.fromList [3])

makeNfa ss ms s fs = NFA (S.fromList ss) (S.fromList ms) s (S.fromList fs)

p2b = makeNfa [0..6]
              [ Emove 0 1, Emove 0 4,
                Move 1 'a' 1, Move 1 'b' 1, Move 1 'a' 2,
                Move 2 'b' 3,
                Move 3 'a' 3, Move 3 'b' 3,
                Move 4 'a' 4, Move 4 'b' 4, Move 4 'b' 5,
                Move 5 'a' 6,
                Move 6 'a' 6, Move 6 'b' 6 ]
              0
              [3,6]
                     
p3a = makeNfa [0..2]
              [Move 0 'a' 0, Move 0 'b' 1, Move 1 'a' 2, Move 2 'a' 0]
              0
              [0]
p3b = makeNfa [0..5]
              [Move 0 'a' 1, Move 0 'b' 3,
               Move 1 'a' 1, Move 1 'b' 2,
               Move 2 'a' 5, Move 2 'b' 2,
               Move 3 'a' 4, Move 3 'b' 3,
               Move 4 'a' 4, Move 4 'b' 5,
               Move 5 'a' 5, Move 5 'b' 5]
              0
              [5]

-------------------------------------------------------------------------- 
--                                                                      --
--      Printing an NFA.                                                --
--                                                                      --
-------------------------------------------------------------------------- 

print_nfa :: Nfa Int -> [Char]

print_nfa (NFA states moves start finish)
      = "States:\t" ++ show_states (S.toList states) ++ "\n\n" ++
        "Moves:\n" ++ (concat (map print_move (S.toList moves))) ++ "\n\n" ++
        "Start:\t" ++ show start ++ "\n\n" ++
        "Finish:\t" ++ show_states (S.toList finish) ++ "\n"

show_states :: [Int] -> [Char]
show_states = concat . (map ((++" ") . show))

print_move :: Move Int -> [Char]
print_move (Move s1 c s2) = "\t" ++ show s1 ++ "\t" ++ [c] ++ "\t"
                            ++ show s2 ++ "\n"
print_move (Emove s1 s2) = "\t" ++ show s1 ++ "\t@\t" ++ show s2 ++ "\n"

-------------------------------------------------------------------------- 
--                                                                      --
--      Printing a set of equivalence classes.                          --
--                                                                      --
-------------------------------------------------------------------------- 

print_classes :: Set (Set Int) -> [Char]
print_classes ss = pcs (map S.toList (S.toList ss))
    where
      pcs = concat . map pc 
      pc  = (++"\n") .  concat . (map ((++"\t").show_int))

show_int :: Int -> [Char]

show_int = show

machA = makeNfa
            [0..2]
            [ Move 0 'a' 1
            , Move 0 'b' 0
            , Move 1 'b' 2
            , Move 1 'a' 1
            , Move 2 'a' 2
            , Move 2 'b' 2
            ]
            0
            [0,1]

machB = makeNfa
            [0..2]
            [ Move 0 'b' 1
            , Move 0 'a' 0
            , Move 1 'a' 2
            , Move 1 'b' 1
            , Move 2 'a' 2
            , Move 2 'b' 2
            ]
            0
            [0,1]            
