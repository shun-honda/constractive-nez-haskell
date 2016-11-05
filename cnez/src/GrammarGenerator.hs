module GrammarGenerator  where

import Value
import Type

generate:: [Value] -> String
generate list = "Start = " ++ (show (tyOf (list !! ((length list) - 1)))) ++ "\n" ++ (gen list)

gen:: [Value] -> String
gen (v:vs) = ret where
  r1 = (gen1 v) ++ "\n"
  ret = r1 ++ gen vs
gen [] = ""

gen1:: Value -> String

gen1 (Token x t) = (show t) ++ " = { " ++ "'" ++ x ++ "'" ++ " #" ++ (show t) ++ " }"
gen1 (Struct v t) = (show t) ++ " = { " ++ (gen_sequence v) ++ " #" ++ (show t) ++ " }"

gen_sequence:: [Value] -> String
gen_sequence (v:vs) = (gen2 v) ++ " " ++ (gen_sequence vs)
gen_sequence [] = ""

gen2:: Value -> String
gen2 (Token x t) = (show t)
gen2 (Struct v t) = (show t)
gen2 (Link v t) = "$(" ++ (gen2 v) ++ ")"
