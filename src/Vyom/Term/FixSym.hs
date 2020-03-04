{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Vyom.Term.FixSym where

import Vyom
import Vyom.Data.TypeRep
import Vyom.Term.LamSym
import Util (maybeToEither)

import GHC.Base
import Data.Kind (Type)

-- The "primitive" (pun intended) way of defining recursive functions
class FixSym r where
  fix :: TypeRep a -> r (a,h) a -> r h a

instance FixSym Run where
  fix _ f = Run $ \h -> let a = run f (a,h) in a

instance FixSym Pretty where
  fix _ f = Pretty $ \i -> "(fix (" ++ unPretty f (i+1) ++ "))"

instance FixSym Expr where
  fix t f = Expr $ Node "Fix" [serialiseTypeRep t, serialise f]

-- Fix which does not require being supplied the type
recurse :: (FixSym r, Typeable a) => r (a,h) a -> r h a
recurse body = fix (argt body) body
  where
    argt :: Typeable a => r (a,h) b -> TypeRep a
    argt _ = typeRep

deserialise :: (LamSym r, FixSym r) => ExtensibleDeserialiser r
deserialise _ self (Node "Fix" [t, e]) env = do
  SomeTypeRep ta <- decode t
  case typeRep @Type `eqTypeRep` typeRepKind ta of
    Just HRefl -> do
      db@(Dyn tb vb) <- self e (ta, env)
      case ta `eqTypeRep` tb of
        Just HRefl -> return $ Dyn ta (fix ta vb)
        _ -> Left $ "Expected type: " ++ show ta ++ " found type: " ++ show tb

    _ -> Left $ "Unexpected type: " ++ show ta

deserialise _ _ (Node "Fix" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise old self e env = old self e env

