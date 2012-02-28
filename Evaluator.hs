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
tiFinal ([sole_addr], [], heap, _, _) = isDataNode $ hLookup heap sole_addr
tiFinal ([], _, _, _, _)              = error "empty stack!"
tiFinal _                             = False

isDataNode :: Node -> Bool
isDataNode (NNum _)    = True
isDataNode (NData _ _) = True
isDataNode _           = False

step :: TiState -> TiState
step state@(a:as, d, heap, g, s) = dispatch $ hLookup heap a
    where dispatch (NNum n)                  = numStep state n
          dispatch (NAp a1 a2)               = apStep state a1 a2
          dispatch (NSupercomb sc args body) = scStep state sc args body
          dispatch (NInd addr)               = (addr : as, d, heap, g, s)
          dispatch (NPrim name prim)         = primStep state prim
          dispatch (NData tag components)    = dataStep state tag components

dataStep :: TiState -> Int -> [Addr] -> TiState
dataStep ([_], stack:dump, heap, globals, stats) _ _ =
    (stack, dump, heap, globals, stats)
dataStep _ _ _ = error "data applied as function"

numStep :: TiState -> Int -> TiState
numStep ([_], stack:dump, heap, globals, stats) _ =
    (stack, dump, heap, globals, stats)
numStep _ _ = error "number applied as a function"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (s@(a:_), d, h, g, st) a1 a2
    | NInd a3 <- n2 = (s, d, hUpdate h a $ NAp a1 a3, g, st)
    | otherwise     = (a1:s, d, h, g, st)
    where n2 = hLookup h a2

scStep :: TiState -> Name -> [Name] -> CoreExp -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body =
    (stack', dump, heap'', globals, stats)
    where stack'                = result_addr : stack_tail
          heap''                = hUpdate heap' ind_elem $ NInd result_addr
          (heap', result_addr)  = instantiate body heap env
          env                   = arg_bindings ++ globals
          arg_bindings          = zip arg_names $ getargs heap stack
          ind_elem : stack_tail = drop (length arg_names) stack

primStep :: TiState -> Primitive -> TiState
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div
primStep state Gt  = primCmp state (>)
primStep state Geq = primCmp state (>=)
primStep state Lt  = primCmp state (<)
primStep state Leq = primCmp state (<=)
primStep state Eq  = primCmp state (==)
primStep state Neq = primCmp state (/=)
primStep state If  = primIf state
primStep state PrimConstr = error "PrimConstr not implemented"

primIf :: TiState -> TiState
primIf (stack@[_,a1,a2,a3], dump, heap, env, stats)
    | isDataNode cond = ([b], dump, hUpdate heap a3 n, env, stats)
    | otherwise       = ([c], [a1,a2,a3]:dump, heap, env, stats)
    where args@[c,b1,b2] = getargs heap stack
          b              = case cond of
                             NData 0 [] -> b2
                             NData 1 [] -> b1
          n              = hLookup heap b
          cond           = hLookup heap c

primCmp :: TiState -> (Int -> Int -> Bool) -> TiState
primCmp state@(_,_,heap,env,_) op = primDyadic state fun
    where fun (NNum n1) (NNum n2) = hLookup heap addr
              where addr          = aLookup env kw $ error $ kw ++ " is not defined"
                    kw            = if op n1 n2 then "True" else "False"

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state op = primDyadic state fun
    where fun (NNum n1) (NNum n2) = NNum $ op n1 n2

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic (stack@[a,a1,a2], dump, heap, env, stats) op
    | bothIsData = ([b'], dump, hUpdate heap' a2 n', env, stats)
    | otherwise  = ([b2], [b1]:[a2]:dump, heap, env, stats)
    where [b1, b2]    = getargs heap stack
          n1          = hLookup heap b1
          n2          = hLookup heap b2
          n'          = op n1 n2
          bothIsData  = isDataNode n1 && isDataNode n2
          (heap', b') = hAlloc heap (op n1 n2)

primNeg :: TiState -> TiState
primNeg ([a,a1], dump, heap, env, stats)
    | isDataNode node = ([b'], dump, hUpdate heap' a1 node', env, stats)
    | otherwise       = ([b], [a1]:dump, heap, env, stats)
    where NAp _ b     = hLookup heap a1
          node        = hLookup heap b
          (heap', b') = instantiate (ENum $ negate n) heap env
          (NNum n)    = node
          node'       = hLookup heap' b'

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) = map get_arg stack
    where get_arg addr = arg where (NAp _ arg) = hLookup heap addr

instantiate :: CoreExp        -- Body of supercombinator
            -> TiHeap         -- Heap before instantiation
            -> [(Name, Addr)] -- Assosiation of names to addresses
            -> (TiHeap, Addr) -- Heap after instantiation, and address
                              --   of root of instance
instantiate (ENum n)    heap env = hAlloc heap $ NNum n
instantiate (EAp e1 e2) heap env = hAlloc heap'' $ NAp a1 a2
    where (heap', a1)  = instantiate e1 heap env
          (heap'', a2) = instantiate e2 heap' env
instantiate (EVar v) heap env = (heap, aLookup env v $ error msg)
    where msg = "Undefined name " ++ show v
instantiate (EConstr tag arity) heap env =
    instantiateConstr tag arity heap env
instantiate (ELet isRec defs body) heap env =
    instantiateLet isRec defs body heap env
instantiate (ECase e alts) heap env =
    error "can't instantiate case yet"

instantiateConstr tag 0 heap env = hAlloc heap $ NData tag []
instantiateConstr tag arity heap env =
    error "can't instantiate constructors with arity /= 0 yet"

instantiateLet isrec defs body heap env = instantiate body heap' env'
    where (heap', env') = foldl' defFolder (heap, env) defs
          defFolder (fheap, fenv) (name, exp) = (fheap', (name, addr):fenv)
              where (fheap', addr) = instantiate exp fheap $ if isrec then env' else fenv
