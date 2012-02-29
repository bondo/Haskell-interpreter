module Gm.Compiler (compile) where

import Data.List (mapAccumL)

import AssocList
import Language
import Heap
import Gm.Utils

compile :: CoreProgram -> GmState
compile program = GmState initialCode [] heap globals statInitial
    where (heap, globals) = buildInitialHeap program

compiledPrimitives = []

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
    where compiled = map compileSc (preludeDefs ++ program) ++ compiledPrimitives

type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
    where (heap', addr) = hAlloc heap $ NGlobal nargs instns

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

compileSc :: (Name, [Name], CoreExp) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body $ zip env [0..])

compileR :: GmCompiler
compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]

type GmCompiler = CoreExp -> GmEnvironment -> GmCode

type GmEnvironment = ASSOC Name Int

compileC :: GmCompiler
compileC (EVar v) env
    | v `elem` aDomain env = [Push n]
    | otherwise            = [Pushglobal v]
    where n = aLookup env v . error $ "Can't happen"
compileC (ENum n) env = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v, m) <- env]