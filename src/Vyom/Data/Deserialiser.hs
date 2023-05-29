module Vyom.Data.Deserialiser where

-- import Vyom.Data.Typ
import Vyom.Data.Dyn
import Type.Reflection
import Vyom.Data.Exp
import Vyom.Data.ErrorOr

-- * e is a compile-time environment: contains types (typeReps)
-- * (RT e) is a run-time environment: contains values
class Var r e where
--  -- The runtime environment corresponding to this compile time environment
  type RT e
  mkvarz :: e -> ErrorOr (Dyn (r (RT e)))
  mkvars :: (forall e'. Var r e' => e' -> ErrorOr (Dyn (r (RT e')))) -> e -> ErrorOr (Dyn (r (RT e)))

-- A Generic Deserialiser
type Deserialiser r = forall a e. Var r e => TypeRep a -> e -> ExprU -> ErrorOr (r (RT e) a)

-- A Generic Extensible Deserialiser
type ExtensibleDeserialiser r = CPSDeserialiser r -> CPSDeserialiser r

-- Compose Multiple ExtensibleDeserialisers together
deserialiseWith :: forall r. [ExtensibleDeserialiser r] -> Deserialiser r
deserialiseWith es tqb env e = do
  Dyn tqa d <- go e env
  case tqa `eqTypeRep` tqb of
    Just HRefl -> return d
    _ -> error "Invalid types"
  where
    go :: DynDeserialiser r
    go = go' es go
    go' :: [ExtensibleDeserialiser r'] -> CPSDeserialiser r'
    go' [] = noRead
    go' (d:ds) = d (go' ds)


-- Private --------------------------------------------------------------------
-- Fallback
noRead :: CPSDeserialiser r
noRead _ e _ = error $ "Malformed Expression: " ++ show e

type DynDeserialiser r = forall e. Var r e => ExprU -> e -> ErrorOr (Dyn (r (RT e)))
type CPSDeserialiser r = DynDeserialiser r -> DynDeserialiser r

getVal ::
  forall x exp env r. Typeable x =>
  (exp -> env -> Either [Char] (Dyn r))
  -> exp -> env -> Either [Char] (r x)
getVal deser e env =
  let tx = typeRep @x in
  do
    b@(Dyn t d) <- deser e env
    case t `eqTypeRep` tx of
      Just HRefl -> return d
      _ -> Left $ "invalid type of argument, expected " <> show tx <> ", found " <> show t



