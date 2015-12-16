{-# LANGUAGE RankNTypes, TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts #-}
module Vyom.Data.Deserialiser where

import Vyom.Data.Typ
import Vyom.Data.Exp
import Vyom.Data.ErrorOr

-- TODO: Make variables more general
type VarName = String
data VarDesc t = VarDesc (TypQ t) VarName

-- * Env is a compile-time environment: contains variable names and types
-- * (RT Env) is a run-time environment: contains values
class Var r e where
  -- The runtime environment corresponding to this compile time environment
  type RT e
  -- Lookup a variable in the environment
  findvar :: VarName -> e -> ErrorOr (Dynamic (r (RT e)))
  mkvarz :: e -> ErrorOr (Dynamic (r (RT e)))
  mkvars :: (forall e'. Var r e' => e' -> ErrorOr (Dynamic (r (RT e')))) -> e -> ErrorOr (Dynamic (r (RT e)))

-- A Generic Deserialiser
type Deserialiser r = forall a e. Var r e => TypQ a -> e -> ExprU -> ErrorOr (r (RT e) a)

-- A Generic Extensible Deserialiser
type ExtensibleDeserialiser r = CPSDeserialiser r -> CPSDeserialiser r

-- Compose Multiple ExtensibleDeserialisers together
deserialiseWith :: forall r. [ExtensibleDeserialiser r] -> Deserialiser r
deserialiseWith es tqb env e = do
  Dynamic tqa d <- go e env
  case gcast tqa tqb d of
    Nothing -> fail "Invalid types"
    Just t -> return t
  where
    go :: DynDeserialiser r
    go = go' es go
    go' :: [ExtensibleDeserialiser r'] -> CPSDeserialiser r'
    go' [] = noRead
    go' (d:ds) = d (go' ds)


-- Private --------------------------------------------------------------------
-- Fallback
noRead :: CPSDeserialiser r
noRead _ e _ = fail $ "Malformed Expression: " ++ show e

type DynDeserialiser r = forall e. Var r e => ExprU -> e -> ErrorOr (Dynamic (r (RT e)))
type CPSDeserialiser r = DynDeserialiser r -> DynDeserialiser r
