module GarbageCollector (gc) where

import Utils
import Heap

import Data.List (foldl')

gc :: TiState -> TiState
gc state@(stack, dump, heap, globals, stats) = (stack, dump, heap', globals, stats)
    where heap' = scanHeap . foldl' markFrom heap $ findRoots state

findRoots :: TiState -> [Addr]
findRoots (s, d, _, g, _) = findStackRoots s ++ findDumpRoots d ++ findGlobalRoots g

findStackRoots :: TiStack -> [Addr]
findStackRoots = id

findDumpRoots :: TiDump -> [Addr]
findDumpRoots = concat

findGlobalRoots :: TiGlobals -> [Addr]
findGlobalRoots = map snd

markFrom :: TiHeap -> Addr -> TiHeap
markFrom heap addr
    | NMarked _ <- node = heap
    | otherwise         = markNode (mark addr node) node
    where mark a n = hUpdate heap a $ NMarked n
          node     = hLookup heap addr

markNode :: TiHeap -> Node -> TiHeap
markNode heap (NAp a1 a2)  = markFrom (markFrom heap a1) a2
markNode heap (NInd a)     = markFrom heap a 
markNode heap (NData _ as) = foldl' markFrom heap as
markNode heap _            = heap

scanHeap :: TiHeap -> TiHeap
scanHeap heap = foldl' unmark heap $ hAddresses heap

unmark :: TiHeap -> Addr -> TiHeap
unmark heap a
    | NMarked n <- hLookup heap a = hUpdate heap a n
    | otherwise                   = hFree heap a
