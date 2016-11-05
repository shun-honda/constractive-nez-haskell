module StructureAbstraction  where

import Value
import Type
import Debug.Trace

abstract :: Value -> [Value] -> [Value]
abstract (Link v t) list = abstract v list

abstract v list = if (contains v list)
  then do
    v_abstract v list
  else
    a_debug r where
      l = v:list
      r = v_abstract v l

a_debug :: Show a => [a] -> [a]
a_debug list = trace ("list:" ++(show list)) list


v_abstract :: Value -> [Value] -> [Value]
v_abstract (Token v t) list = list

v_abstract (Sequence vs t) list = ret where
  ret = abstruct_sequence vs list

v_abstract (Struct vs t) list = ret where
  ret = abstruct_sequence vs list

v_abstract (Link v t) list = abstract v list

v_abstract Error list = fail "inference error"


abstruct_sequence :: [Value] -> [Value] -> [Value]
abstruct_sequence (v:vs) list = r2 where
  r1 = abstruct_sequence vs list
  r2 = abstract v r1

abstruct_sequence [] list = list


contains :: Value -> [Value] -> Bool
contains v (x:xs) = if v `typeEq` x
  then True
  else contains v xs

contains v [] = False
