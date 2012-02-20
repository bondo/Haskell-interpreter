module Utils where

import Language
import Heap

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
data TiDump = DummyTiDump deriving (Show)
type TiHeap = Heap Node

data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExp
          | NNum Int
          | NInd Addr
            deriving (Show)

type TiGlobals = [(Name, Addr)]

type TiStats = Int
tiStatInitial :: TiStats
tiStatInitial = 0
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s + 1
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (s, d, h, g, st) = (s, d, h, g, f st)

initialTiDump = DummyTiDump

showResults :: [TiState] -> String
showResults states = concatMap showState states ++ showStats (last states)

showResult :: [TiState] -> String
showResult states = res ++ showStats last_state
    where last_state@([addr], _, heap, _, stats) = last states
          res = "Result = " ++ showStkNode heap (hLookup heap addr)

showState :: TiState -> String
showState (stack, dump, heap, globals, stats) = showStack heap stack ++ "\n"

showStack :: TiHeap -> TiStack -> String
showStack heap stack = "Stk [" ++ pretty_stack ++ "]"
    where pretty_stack = concatMap show_stack_item stack
          show_stack_item addr = "\n\t" ++ show addr ++ ": " ++ node addr
          node addr = showStkNode heap $ hLookup heap addr

showStkNode :: TiHeap -> Node -> String
showStkNode heap (NAp fun_addr arg_addr) =
    "NAp " ++ fun ++ " " ++ arg ++ " (" ++ node ++ ")"
    where fun  = show fun_addr
          arg  = show arg_addr
          node = showNode $ hLookup heap arg_addr
showStkNode _ node = showNode node

showNode :: Node -> String
showNode (NAp a1 a2) = "NAp " ++ show a1 ++ " " ++ show a2
showNode (NSupercomb name _ _) = "NSupercomb " ++ name
showNode (NNum n) = "NNum " ++ show n
showNode (NInd a) = "NInd " ++ show a

showStats :: TiState -> String
showStats (_,_,_,_, stats) =
    "\nTotal number of steps = " ++ show (tiStatGetSteps stats)