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
  db <- self e1 env
  b <- unpackBool db
  Dynamic t1 d1 <- self e2 env
  Dynamic t2 d2 <- self e3 env
  d1' <- maybeToEither ("Could not unify type: " ++ show t2 ++ " with type: " ++ show t1) $ gcast t1 t2 d1
  return $ Dynamic t2 (cond b d1' d2)
  where
    unpackBool b@(Dynamic t _) = maybeToEither ("Invalid type of argument, expected Bool, found " ++ show t) $ asBool b
deserialise _ _ (Node "Cond" es) _ = Left $ "Invalid number of arguments, expected 3, found " ++ show (length es)

deserialise old self e env = old self e env
