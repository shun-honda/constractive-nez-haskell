module ConstructiveNez
    ( infer
    ) where

import StructureInference
import StructureAbstraction
import GrammarGenerator
import Type
import Value

tokenize :: String -> [Value]
tokenize s = [(Token "\"name\"" TString), (Token ":" TKeyValueDelim), (Token "\"Taro\"" TString)]

infer :: IO ()
infer = do
  let tokens = (tokenize "name:Taro")
  let v = sinfer tokens
  let list = abstract v []
  print tokens
  print $ tyOf (tokens !! 0)
  print v
  print list
  putStrLn $ generate list
