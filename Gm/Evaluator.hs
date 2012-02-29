module Gm.Evaluator (eval) where

import Language
import AssocList
import Heap
import Gm.Utils

import Data.List

eval :: GmState -> [GmState]
eval state = state : restStates
    where restStates | gmFinal state = []
                     | otherwise     = eval nextState
          nextState  = doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin s = s { gmStats = statIncSteps $ gmStats s }

gmFinal :: GmState -> Bool
gmFinal s | [] <- gmCode s = True
gmFinal _                  = False

step :: GmState -> GmState
step state = dispatch i $ state { gmCode = is }
    where i:is = gmCode state

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind

pushglobal :: Name -> GmState -> GmState
pushglobal f s = s { gmStack = a : gmStack s }
    where a = aLookup (gmGlobals s) f . error $ "Undeclared global " ++ f

pushint :: Int -> GmState -> GmState
pushint n s = s { gmStack = a : gmStack s, gmHeap = heap' }
    where (heap', a) = hAlloc (gmHeap s) $ NNum n

mkap :: GmState -> GmState
mkap s = s { gmStack = a:as', gmHeap = heap' }
    where (heap', a) = hAlloc (gmHeap s) $ NAp a1 a2
          a1:a2:as'  = gmStack s

push :: Int -> GmState -> GmState
push n s = s { gmStack = a:as }
    where as = gmStack s
          a  = getArg . hLookup (gmHeap s) $ as !! (n+1)
          getArg (NAp _ a) = a

slide :: Int -> GmState -> GmState
slide n s = s { gmStack = a : drop n as }
    where a:as = gmStack s

unwind :: GmState -> GmState
unwind s = newState . hLookup (gmHeap s) $ a
    where a:as                   = gmStack s
          newState (NNum n)      = s
          newState (NAp a1 _)    = s { gmCode = [Unwind], gmStack = a1:a:as }
          newState (NGlobal n c) = if length as < n
                                   then error "Unwinding too few arguments"
                                   else s { gmCode = c }