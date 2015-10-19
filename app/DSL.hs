{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, ConstraintKinds #-}
module DSL
( DSL
, serialise
, deserialise
, pretty
, run
, module Vyom.Term.StringSym
, module Vyom.Term.LamSym
, module Vyom.Term.AppSym
, module Vyom.Term.FixSym
, module Vyom.Term.IntSym
, module Vyom.Term.BoolSym
, module Vyom.Term.CondSym
, module Vyom.Term.UnitSym
, module Vyom.Term.TupleSym
, module Vyom.Term.ListSym
) where

import Vyom

import Vyom.Term.StringSym hiding (deserialise)
import Vyom.Term.LamSym hiding (deserialise)
import Vyom.Term.AppSym hiding (deserialise)
import Vyom.Term.FixSym hiding (deserialise)
import Vyom.Term.IntSym hiding (deserialise)
import Vyom.Term.BoolSym hiding (deserialise)
import Vyom.Term.UnitSym hiding (deserialise)
import Vyom.Term.CondSym hiding (deserialise)
import Vyom.Term.TupleSym hiding (deserialise)
import Vyom.Term.ListSym hiding (deserialise)

import qualified Vyom.Term.StringSym (deserialise)
import qualified Vyom.Term.LamSym (deserialise)
import qualified Vyom.Term.AppSym (deserialise)
import qualified Vyom.Term.FixSym (deserialise)
import qualified Vyom.Term.IntSym (deserialise)
import qualified Vyom.Term.BoolSym (deserialise)
import qualified Vyom.Term.UnitSym (deserialise)
import qualified Vyom.Term.CondSym (deserialise)
import qualified Vyom.Term.TupleSym (deserialise)
import qualified Vyom.Term.ListSym (deserialise)

-- Compose various parts
type Sym r =
  ( LamSym    r
  , AppSym    r
  , FixSym    r
  , IntSym    r
  , BoolSym   r
  , StringSym r
  , UnitSym   r
  , CondSym   r
  , TupleSym  r
  , ListSym   r
  )

-- A Full Language term
-- 'rtenv' is the *RunTime* environment
-- 'a' is the output type
type DSL rtenv a = forall r. Sym r => r rtenv a

-- This gives us the following automatically -
--   pretty :: Term env a -> String
--   run :: Term env a -> env -> ErrorOr a
--   serialise :: Term env a -> ExprU

-- Typed deserialisation
-- This currently can't be derived automatically
-- Layer the individual deserialisers
-- Effectively:
--   deserialise :: TypQ a -> env -> ExprU -> ErrorOr (Term rtenv a)
--   Using it in that manner poses no problems
--   But we can't write that signature as-is due to RankN typing issues
deserialise :: Sym r => Deserialiser r
deserialise = deserialiseWith
    [ Vyom.Term.AppSym.deserialise
    , Vyom.Term.BoolSym.deserialise
    , Vyom.Term.CondSym.deserialise
    , Vyom.Term.FixSym.deserialise
    , Vyom.Term.IntSym.deserialise
    , Vyom.Term.LamSym.deserialise
    , Vyom.Term.ListSym.deserialise
    , Vyom.Term.StringSym.deserialise
    , Vyom.Term.TupleSym.deserialise
    , Vyom.Term.UnitSym.deserialise
    ]
