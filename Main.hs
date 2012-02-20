module Main where

import Language
import Pretty
import Parser
import Compiler
import Utils
import Evaluator

testProgram = "test arg1 arg2 = arg1 arg2"

testProgram2 = "f = 3 ;\n\
               \g x y = let z = x in z ;\n\
               \h x = case (let y = x in y) of\n\
                       \{ <1> -> 2 \n\
                       \; <2> -> 5 \n\
                       \}"

testProgram3 = "main = S K K 3" -- main = 3

testProgram4 = "\
\pair x y f = f x y ; \n\
\fst p = p K ; \n\
\snd p = p K1 ; \n\
\f x y = let rec \n\
\            a = pair x b ; \n\
\            b = pair y a \n\
\        in \n\
\        fst (snd (snd (snd a))) ; \n\
\main = f 3 4" -- main = 4

testProgram5 = "main = let rec a = 101 in 42"

testProgram6 = "f x = x ; main = let a = 42 in f a"

testProgram7 = "id x = x ; main = twice twice twice id 3"

testChurch = "succ = S (S (K S) K) ; \
             \add  = S I (K (S ((S (K S)) K))) ; \
             \mul  = S (K S) K ; \
             \pow  = I ; \
             \ch0  = I ; \
             \ch1  = succ ch0 ; \
             \ch2  = succ ch1 ; \
             \ch3  = succ ch2 ; \
             \ch4  = succ ch3 ; \
             \ch5  = succ ch4 ; \
             \main = add ch1 ch2"

-- parse and pretty-print
papp = putStrLn . either show (show . pretty) . parseCore

-- parse and eval
pae = eval . make . parseCore
    where make = either (error . show) compile

trace = putStrLn . showResults . pae

run = putStrLn . showResult . pae

main = papp testProgram