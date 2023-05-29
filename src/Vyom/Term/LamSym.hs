module Vyom.Term.LamSym where

import Data.Kind (Type)

import Vyom
import Vyom.Data.TypeRep

-- Lambdas with tuple runtime environments
class LamSym r where
  -- TODO: Rename s and z to something longer
  z   :: r (a,h) a
  s   :: r h a -> r (any,h) a
  -- Need to pass the type of elements, because we don't have type inference
  -- Hence lam needs to know the type of the var to allow type checking
  -- TODO: Implement type inference
  lam :: TypeRep a -> r (a,h) b -> r h (a -> b)

lambda :: (LamSym r, Typeable a) => r (a,h) b -> r h (a -> b)
lambda body = lam typeRep body

-- Helpful synonyms
-- infixr 0 #\
(#\) :: forall r h a b. Typeable a => LamSym r => r (a,h) b -> r h (a -> b)
(#\) = lam (typeRep @a)
v0 :: LamSym r => r (a,h) a
v0 = z
v1 :: LamSym r => r (any,(a,h)) a
v1 = s z
v2 :: LamSym r => r (any,(any,(a,h))) a
v2 = s $ s z
v3 :: LamSym r => r (any,(any,(any,(a,h)))) a
v3 = s $ s $ s z
v4 :: LamSym r => r (any,(any,(any,(any,(a,h))))) a
v4 = s $ s $ s $ s z

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
  lam t e = Expr $ Node "Lam" [Leaf "anonymous", serialiseTypeRep t, serialise e]

-- Instance computations for tuples
instance LamSym r => Var r () where
  type RT () = ()
  mkvarz _ = Left "Unbound variable"
  mkvars getter _ = getter ()

instance (LamSym r, Var r e) => Var r (TypeRep (a :: Type), e) where
  type RT (TypeRep a, e) = (a, RT e)

  mkvarz (tr, _) = return $ Dyn tr z

  mkvars getter (_, env) = do
    Dyn t d <- getter env
    return $ Dyn t (s d)

deserialise :: LamSym r => ExtensibleDeserialiser r

deserialise _ _ (Node "Z" []) env = mkvarz env
deserialise _ _ (Node "Z" es) _ = Left $ "Invalid number of arguments, expected 0, found " ++ show (length es)

deserialise _ self (Node "S" [e1]) env = mkvars (self e1) env
deserialise _ _ (Node "S" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ self (Node "Lam" [Leaf _, t1, e1]) env = do
  -- e1 *should be* a term that expects an env of (a,env)
  -- Our deserialiser function needs to know the type of the environment
  -- But we have been provided an env of env
  -- So we need to figure out typerep a
  -- TODO: Currently there is no way to type infer `a` from the body
  SomeTypeRep rep <- decode t1
  case typeRep @Type `eqTypeRep` typeRepKind rep of
    Just HRefl -> do
      Dyn tbody body <- self e1 (rep, env)
      return $ Dyn (Fun rep tbody) (lam rep body)
    Nothing -> Left $ "Invalid type"

deserialise _ _ (Node "Lam" es) _ = Left $ "Invalid number of arguments, expected 3, found " ++ show (length es)

deserialise old self e env = old self e env

