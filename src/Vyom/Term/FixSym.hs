{-# LANGUAGE RankNTypes #-}
module Vyom.Term.FixSym where

import Vyom
import Vyom.Term.LamSym
import Util (maybeToEither)

-- The "primitive" (pun intended) way of defining recursive functions
class FixSym r where
  fix :: TypQ a -> r (a,h) a -> r h a

instance FixSym Run where
  fix _ f = Run $ \h -> let a = run f (a,h) in a

instance FixSym Pretty where
  fix _ f = Pretty $ \i -> "(fix (" ++ unPretty f (i+1) ++ "))"

instance FixSym Expr where
  fix t f = Expr $ Node "Fix" [typToExp (Typ t), serialise f]

-- Fix which does not require being supplied the type
recurse :: (SupportedType a, FixSym r) => r (a,h) a -> r h a
recurse body = fix (argt body) body
  where
    argt :: SupportedType a => r (a,h) b -> TypQ a
    argt _ = typrep

deserialise :: (LamSym r, FixSym r) => ExtensibleDeserialiser r
deserialise _ self (Node "Fix" [t, e]) env = do
  Typ ta <- expToTyp t
  Dynamic tb d <- self e (VarDesc ta "dummy", env)
  d' <- maybeToEither ("Expected type: " ++ show ta ++ " found type: " ++ show tb) $ gcast tb ta d
  return $ Dynamic ta (fix ta d')
deserialise _ _ (Node "Fix" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise old self e env = old self e env

