module Vyom.Term.CondSym where

import Vyom
import Util (maybeToEither)

-- Conditionals
-- If cond then x else y
class CondSym r where
  cond :: r h Bool -> r h a -> r h a -> r h a

instance CondSym Run where
  cond = rop3 if'
    where
    if' :: Bool -> a -> a -> a
    if' c a b = if c then a else b

instance CondSym Pretty where
  cond = sop3 "if" "then" "else"

instance CondSym Expr where
  cond = eop3 "Cond"

deserialise :: CondSym r => ExtensibleDeserialiser r
deserialise _ self (Node "Cond" [e1, e2, e3]) env = do
  b <- getVal @Bool self e1 env
  Dyn t1 d1 <- self e2 env
  Dyn t2 d2 <- self e3 env
  case t1 `eqTypeRep` t2 of
    Just HRefl -> return $ Dyn t1 $ cond b d1 d2
    _ -> Left $ "Could not unify type: " <> show t2 <> " with type: " <> show t1

deserialise _ _ (Node "Cond" es) _ = Left $ "Invalid number of arguments, expected 3, found " <> show (length es)

deserialise old self e env = old self e env

