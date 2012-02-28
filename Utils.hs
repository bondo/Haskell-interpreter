module Utils where

import Language
import Heap

import Data.List (sort, intercalate)

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
type TiDump = [TiStack]
type TiHeap = Heap Node

data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExp
          | NNum Int
          | NInd Addr
          | NPrim Name Primitive
          | NData Int [Addr]
            deriving (Show)

data Primitive = Neg | Add | Sub | Mul | Div
               | Gt | Geq | Lt | Leq | Eq | Neq
               | PrimConstr | If
                 deriving (Show, Eq)

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

initialTiDump = []

showResults :: [TiState] -> String
showResults states = concatMap showState states ++ showStats (last states)

showResult :: [TiState] -> String
showResult states = res ++ showStats last_state
    where last_state@([addr], _, heap, _, stats) = last states
          res = "Result = " ++ showStkNode heap (hLookup heap addr)

showState :: TiState -> String
showState (stack, dump, heap, globals, stats) =
    prettyStack ++ "\n" ++ prettyDump ++ "\n" ++ prettyHeap ++ "\n\n"
    where prettyStack = showStack heap stack
          prettyDump  = showDump heap dump
          prettyHeap  = showHeap heap

showStack :: TiHeap -> TiStack -> String
showStack heap stack = "Stk " ++ showAddrs heap stack

showHeap :: TiHeap -> String
showHeap heap = "Heap " ++ shortDesc -- (showAddrs heap . sort . hAddresses $ heap)
    where shortDesc = "[" ++ (show . length $ hAddresses heap) ++ " elements]"

showDump :: TiHeap -> TiDump -> String
showDump heap dump = "Dump [[ " ++ intercalate "\n  " (map (showStack heap) dump) ++ " ]]"

showAddrs :: TiHeap -> [Addr] -> String
showAddrs heap addrs     = "[" ++ pretty_addrs ++ "]"
    where pretty_addrs   = concatMap show_item addrs
          show_item addr = "\n\t" ++ show addr ++ ": " ++ node addr
          node addr      = showStkNode heap $ hLookup heap addr


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
showNode (NPrim n p) = "NPrim " ++ n
showNode (NData tag _) = "NData " ++ show tag ++ " [...]"

showStats :: TiState -> String
showStats (_,_,_,_, stats) =
    "\nTotal number of steps = " ++ show (tiStatGetSteps stats)