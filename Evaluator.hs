module Evaluator where

import Heap
import Utils
import Language
import AssocList

import Data.List

eval :: TiState -> [TiState]
eval state = state : rest_states
    where rest_states | tiFinal state = []
                      | otherwise     = eval next_state
          next_state  = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin = applyToStats tiStatIncSteps

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], _, heap, _, _) = isDataNode (hLookup heap sole_addr)
tiFinal ([], _, _, _, _)             = error "empty stack!"
tiFinal _                            = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step state@(stack, _, heap, _, _) = dispatch $ hLookup heap $ head stack
    where dispatch (NNum n)                  = numStep state n
          dispatch (NAp a1 a2)               = apStep state a1 a2
          dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep _ _ = error "number applied as a function"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (s, d, h, g, st) a1 a2 = (a1 : s, d, h, g, st)

scStep :: TiState -> Name -> [Name] -> CoreExp -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body =
    (new_stack, dump, new_heap, globals, stats)
    where new_stack = result_addr : drop (length arg_names + 1) stack
          (new_heap, result_addr) = instantiate body heap env
          env = arg_bindings ++ globals
          arg_bindings = zip arg_names (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) = map get_arg stack
    where get_arg addr  = arg where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExp        -- Body of supercombinator
            -> TiHeap         -- Heap before instantiation
            -> [(Name, Addr)] -- Assosiation of names to addresses
            -> (TiHeap, Addr) -- Heap after instantiation, and address
                              --   of root of instance
instantiate (ENum n)    heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap'' (NAp a1 a2)
    where (heap', a1)  = instantiate e1 heap env
          (heap'', a2) = instantiate e2 heap' env
instantiate (EVar v) heap env = (heap, aLookup env v (error msg))
    where msg = "Undefined name " ++ show v
instantiate (EConstr tag arity) heap env =
    instantiateConstr tag arity heap env
instantiate (ELet isRec defs body) heap env =
    instantiateLet isRec defs body heap env
instantiate (ECase e alts) heap env =
    error "can't instantiate constructors yet"

instantiateConstr tag arity heap env =
    error "can't instantiate constructors yet"

instantiateLet isRec defs body heap env = instantiate body heap' env'
    where (heap', env') = foldl' defFolder (heap, []) defs
          defFolder (h, e) (name, exp) = (h', (name, addr):e)
              where (h', addr) = instantiate exp h env_to_use
                    env_to_use = if isRec then e else env
