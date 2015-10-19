{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Vyom.Term.LamSym where

-- TODO: This implementation is a mess! Make it more generic.

import Vyom

-- Lambdas with (runtime) environments
class LamSym r where
  -- TODO: Rename s and z to something longer
  z   :: r (a,h) a
  s   :: r h a -> r (any,h) a
  -- Need to pass the type of elements, because we don't have type inference
  -- Hence lam needs to know the type of the var to allow type checking
  -- TODO: Implement type inference
  lam :: TypQ a -> r (a,h) b -> r h (a -> b)

instance LamSym Run where
  z     = Run fst
  s v   = Run $ run v . snd
  lam _ e = Run $ \h x -> run e (x,h)

instance LamSym Pretty where
  z = Pretty $ \i -> "x" ++ show (i-1)
  s v = Pretty $ \i -> unPretty v (i-1)
  lam _ e = Pretty $ \i -> "(\\x" ++ show i ++ " -> " ++ unPretty e (i+1) ++ ")"

instance LamSym Expr where
  z = Expr $ Node "Z" []
  s v = Expr $ Node "S" [serialise v]
  lam t e = Expr $ Node "Lam" [Leaf "dummy", typToExp (Typ t), serialise e]

-- EXPERIMENTAL --
-- Create an ExprU with a varname
var :: String -> Expr h a
var name = Expr $ Node "Var" [Leaf name]

-- Create an ExprU with a lambda
-- The body should use `var` to refer to named variables
lambda :: String -> TypQ a -> Expr (a,h) b -> Expr h (a -> b)
lambda name typ body = Expr $ Node "Lam" [Leaf name, typToExp (Typ typ), serialise body]
-- END EXPERIMENTAL --


-- Instance computations for tuples
instance LamSym r => Var r () where
  type RT () = ()
  findvar name _ = Left $ "Unbound variable: " ++ show name
  mkvarz _ = Left "Unbound variable"
  mkvars self () = self ()

instance (LamSym r, Var r env) => Var r (VarDesc t, env) where
  type RT (VarDesc t, env) = (t, RT env)

  findvar name (VarDesc tr name', _) | name == name' = return $ Dynamic tr z

  findvar name (_,env) = do
    Dynamic tr v <- findvar name env
    return $ Dynamic tr (s v)

  mkvarz (VarDesc tr _, _) = return $ Dynamic tr z

  mkvars self (_, env) = do
    Dynamic t d <- self env
    return $ Dynamic t (s d)


deserialise :: LamSym r => ExtensibleDeserialiser r

deserialise _ _ (Node "Z" []) env = mkvarz env
deserialise _ _ (Node "Z" es) _ = Left $ "Invalid number of arguments, expected 0, found " ++ show (length es)

deserialise _ self (Node "S" [e1]) env = mkvars (self e1) env
deserialise _ _ (Node "S" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ _ (Node "Var" [Leaf name]) env = findvar name env
deserialise _ _ (Node "Var" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ self (Node "Lam" [Leaf _, t, e1]) env = do
  Typ ta <- expToTyp t
  Dynamic tbody body <- self e1 (VarDesc ta "dummy", env)
  return $ Dynamic (tarr ta tbody) (lam ta body)
deserialise _ _ (Node "Lam" es) _ = Left $ "Invalid number of arguments, expected 3, found " ++ show (length es)

deserialise old self e env = old self e env
