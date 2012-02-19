module Compiler where

import Utils
import Language
import AssocList
import Heap

import Data.List (mapAccumL)

compile :: CoreProgram -> TiState
compile program =
    (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
    where
      sc_defs = program ++ preludeDefs ++ extraPreludeDefs
      (initial_heap, globals) = buildInitialHeap sc_defs
      initial_stack = [address_of_main]
      address_of_main = aLookup globals "main" (error no_main)
      no_main = "main is not defined"

buildInitialHeap :: [CoreScDef] -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccumL allocateSc hInitial

allocateSc :: TiHeap -> CoreScDef -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
    where (heap', addr) = hAlloc heap (NSupercomb name args body)