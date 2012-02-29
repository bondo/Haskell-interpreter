module Main where

import Language
import Pretty
import Parser
import Gm.Compiler
import Gm.Utils
import Gm.Evaluator

testProgram = "test arg1 arg2 = arg1 arg2"

testProgram2 = "f = 3 ;\n\
               \g x y = let z = x in z ;\n\
               \h x = case (let y = x in y) of\n\
                       \{ <1> -> 2 \n\
                       \; <2> -> 5 \n\
                       \}"

testProgram3 = "main = S K K 42"

testProgram4 = "\
\pair x y f = f x y ; \n\
\fst p = p K ; \n\
\snd p = p K1 ; \n\
\f x y = let rec \n\
\            a = pair x b ; \n\
\            b = pair y a \n\
\        in \n\
\        fst (snd (snd (snd a))) ; \n\
\main = f 3 42"

testProgram5 = "main = let rec a = 101 in 42"

testProgram6 = "f x = x ; main = let a = 42 in f a"

testProgram7 = "id x = x ; main = twice twice twice id 42"

testProgram8 = "main = negate -42"

testProgram9 = "main = twice negate 42"

testProgram10 = "main = mul 7 6"

testProgram11 = "main = plus (negate 10) 52"

testProgram12 = "main = minus 46 (div 8 2)"

testProgram13 = "fac n = if (eq n 0) 1 (mul n (fac (minus n 1))) ; main = plus (fac 4) 18"

-- parse and pretty-print
papp = putStrLn . either show (show . pretty) . parseCore

-- parse and eval
pae = eval . make . parseCore
    where make = either (error . show) compile

trace = putStrLn . showResults . pae

--run = putStrLn . showResult . pae

main = papp testProgram
