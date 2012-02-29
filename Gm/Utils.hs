module Gm.Utils where

import Data.List (intercalate)

import AssocList
import Heap
import Language

data GmState = GmState { gmCode    :: GmCode    -- Current instruction stream
                       , gmStack   :: GmStack   -- Current stack
                       , gmHeap    :: GmHeap    -- Heap of nodes
                       , gmGlobals :: GmGlobals -- Global addresses in heap
                       , gmStats   :: GmStats   -- Statistics
                       }

type GmCode = [Instruction]
type GmStack = [Addr]
type GmHeap = Heap Node
type GmGlobals = ASSOC Name Addr

data Instruction
    = Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
    | Slide Int
      deriving (Eq, Show)

data Node
    = NNum Int
    | NAp Addr Addr
    | NGlobal Int GmCode
      deriving (Show)

type GmStats = Int
statInitial :: GmStats
statInitial = 0
statIncSteps :: GmStats -> GmStats
statIncSteps s = s + 1
statGetSteps :: GmStats -> Int
statGetSteps s = s

showResults :: [GmState] -> String
showResults states = "Supercombinator definitions\n\n" ++ scs ++ "\n\nState transitions\n\n" ++ sts ++ end
    where scs = intercalate "\n" $ map (showSC s) (gmGlobals s)
          sts = intercalate "\n" $ map showState states
          end = "\n\n" ++ showStats (last states)
          s:_ = states

showSC :: GmState -> (Name, Addr) -> String
showSC s (name, addr) = "Code for " ++ name ++ ":\n" ++ showInstructions code
    where (NGlobal arity code) = hLookup (gmHeap s) addr

showInstructions :: GmCode -> String
showInstructions is = "  " ++ intercalate "\n  " (map show is) ++ "\n"

showState :: GmState -> String
showState s = showStack s ++ "\n" ++ showInstructions (gmCode s) ++ "\n"

showStack s = "Stack:[" ++ intercalate "\n       " (map (showStackItem s) . reverse $ gmStack s) ++ "]"

showStackItem :: GmState -> Addr -> String
showStackItem s a = show a ++ ": " ++ showNode s a (hLookup (gmHeap s) a)


showNode :: GmState -> Addr -> Node -> String
showNode s a (NNum n) = show n
showNode s a (NGlobal n g) = "Global " ++ show (head [n | (n,b) <- gmGlobals s, a==b])
showNode s a (NAp a1 a2) = "Ap " ++ show a1 ++ " " ++ show a2

showStats :: GmState -> String
showStats s = "Steps taken = " ++ (show . statGetSteps $ gmStats s)
