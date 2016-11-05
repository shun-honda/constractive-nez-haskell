module Value
  ( tyOf,
    typeEq,
    Value(..)
  ) where

import Type

data Value =
  Token String Type
  | Link Value Type
  | Struct [Value] Type
  | Sequence [Value] Type
  | Error
  deriving (Show)


tyOf :: Value -> Type
tyOf (Token v t) = t
tyOf (Link v t) = t
tyOf (Struct v t) = t
tyOf (Sequence v t) = t

typeEq :: Value -> Value -> Bool
typeEq v1 v2 = (tyOf v1) == (tyOf v2)
