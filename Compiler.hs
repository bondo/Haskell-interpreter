module Compiler where

import Utils
import Language
import AssocList
import Heap

import Data.List (mapAccumL)

compile :: CoreProgram -> TiState
compile prog = (init_stack, initialTiDump, init_heap, globals, tiStatInitial)
    where sc_defs              = prog ++ preludeDefs ++ extraPreludeDefs
          (init_heap, globals) = buildInitialHeap sc_defs
          init_stack           = [address_of_main]
          address_of_main      = aLookup globals "main" $ error no_main
          no_main              = "main is not defined"

buildInitialHeap :: [CoreScDef] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs       = (heap'', sc_addrs ++ prim_addrs)
    where (heap', sc_addrs)    = mapAccumL allocateSc hInitial sc_defs
          (heap'', prim_addrs) = mapAccumL allocatePrim heap' primitives

allocateSc :: TiHeap -> CoreScDef -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
    where (heap', addr)            = hAlloc heap $ NSupercomb name args body

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
    where (heap', addr)        = hAlloc heap $ NPrim name prim

primitives :: [(Name, Primitive)]
primitives = [("negate", Neg),
              ("+", Add), ("-", Sub),
              ("*", Mul), ("/", Div)]
