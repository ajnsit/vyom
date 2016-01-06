module Vyom.Term.IntSym where

import Vyom
import Util (maybeToEither, safeRead)

-- Integer operations
class IntSym r where
  int  :: Int -> r h Int
  add  :: r h Int -> r h Int -> r h Int
  mul  :: r h Int -> r h Int -> r h Int
  gte  :: r h Int -> r h Int -> r h Bool

-- Helpful synonyms
infixl 4 #>=
infixl 6 #+
infixl 7 #*
(#+) :: IntSym r => r h Int -> r h Int -> r h Int
a #+ b = add a b
(#*) :: IntSym r => r h Int -> r h Int -> r h Int
a #* b = mul a b
(#>=) :: IntSym r => r h Int -> r h Int -> r h Bool
a #>= b = gte a b

instance IntSym Run where
  int = rop0
  add = rop2 (+)
  mul = rop2 (*)
  gte = rop2 (>=)

instance IntSym Pretty where
  int = sop0
  add = sop2 "+"
  mul = sop2 "*"
  gte = sop2 ">="

instance IntSym Expr where
  int = eop0 "Int"
  add = eop2 "Add"
  mul = eop2 "Mul"
  gte = eop2 "Gte"


deserialise :: IntSym r => ExtensibleDeserialiser r
deserialise _ _ (Node "Int" [Leaf i]) _
  | Just i' <- safeRead i = return $ Dynamic tint $ int i'
  | otherwise = Left $ "Bad int literal " ++ i
deserialise _ _ (Node "Int" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ self (Node "Add" [e1,e2]) env = do
  i1 <- getInt self e1 env
  i2 <- getInt self e2 env
  return $ Dynamic tint $ add i1 i2
deserialise _ _ (Node "Add" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise _ self (Node "Mul" [e1,e2]) env = do
  i1 <- getInt self e1 env
  i2 <- getInt self e2 env
  return $ Dynamic tint $ mul i1 i2
deserialise _ _ (Node "Mul" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise _ self (Node "Gte" [e1,e2]) env = do
  i1 <- getInt self e1 env
  i2 <- getInt self e2 env
  return $ Dynamic tbool $ gte i1 i2
deserialise _ _ (Node "Gte" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise old self e env = old self e env

-- Private
getInt
  :: (exp -> env -> Either [Char] (Dynamic r))
  -> exp -> env -> Either [Char] (r Int)
getInt deser e env = do
  i@(Dynamic t d) <- deser e env
  maybeToEither ("invalid type of argument, expected bool, found " ++ show t) $ asInt i
