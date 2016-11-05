module StructureInference  where

import Value
import Type

type Context = (Value, [Value])
type ConstructiveFunc = Value -> Type -> [Value] -> Context

sinfer :: [Value] -> Value
sinfer list = do
  let (v, vs) = construct c_sequence list
  v

construct :: (ConstructiveFunc) -> [Value] -> Context
construct f ([]) = (Error, [])
construct f (v:[]) = (f v (tyOf v) [])
construct f (v:vs) = (f v (tyOf v) vs)

c_sequence :: ConstructiveFunc
c_sequence v t vs = alt1
  -- ((Struct (Sequence [v1, v2] (TSequence [tyOf(v1), tyOf(v2)])) (TSequence [tyOf(v1), tyOf(v2)])), r2)
  where
    alt1 = case m1 of
      Error -> (m1, vs)
      otherwise -> case m2 of
        Error -> (m1, vs)
        otherwise -> ((Struct [(Link m1 (tyOf m1)), (Link m2 (tyOf m2))] (TSequence [tyOf(m1), tyOf(m2)])), vs2)
        where
          (m2, vs2) = construct c_keyvalue vs1
      where
        (m1, vs1) = construct c_keyvalue (v:vs)

c_keyvalue:: ConstructiveFunc
c_keyvalue v t vs = alt1
  where
    alt1 = case m1 of
      Error -> (m1, vs1)
      otherwise -> case m2 of
        Error -> (m1, vs1)
        otherwise -> case m3 of
          Error -> (m1, vs1)
          otherwise -> ((Struct [(Link m1 (tyOf m1)), m2, (Link m3 (tyOf m3))] TKeyValue), vs3)
          where
            (m3, vs3) = construct c_bstruct vs2
        where
          (m2, vs2) = construct c_keyvaluedelim vs1
      where
        (m1, vs1) = construct c_bstruct (v:vs)

c_bstruct :: ConstructiveFunc
c_bstruct v TString vs = (v, vs)
c_bstruct v t vs = (Error, v:vs)

c_keyvaluedelim :: ConstructiveFunc
c_keyvaluedelim v TKeyValueDelim vs = (v, vs)
c_keyvaluedelim v t vs = (Error, v:vs)

-- c_or :: (ConstructiveFunc) -> (ConstructiveFunc) -> [Value] -> Context
-- c_or f g list =
--   let (m, vs) = construct f list
--   in case (m) of
--   Error -> (construct g list)
--   otherwise -> (m, vs)
