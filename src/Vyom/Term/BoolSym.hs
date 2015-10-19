module Vyom.Term.BoolSym where

import Vyom
import Util (maybeToEither, safeRead)

-- Boolean Operations
class BoolSym r where
  bool :: Bool -> r h Bool
  andb :: r h Bool -> r h Bool -> r h Bool
  orrb :: r h Bool -> r h Bool -> r h Bool
  negb :: r h Bool -> r h Bool

instance BoolSym Run where
  bool = rop0
  andb = rop2 (&&)
  orrb = rop2 (||)
  negb = rop1 not

instance BoolSym Pretty where
  bool = sop0
  andb = sop2 "&&"
  orrb = sop2 "||"
  negb = sop1 "not"

instance BoolSym Expr where
  bool = eop0 "Bool"
  andb = eop2 "AndB"
  orrb = eop2 "OrrB"
  negb = eop1 "NegB"

deserialise :: BoolSym r => ExtensibleDeserialiser r
deserialise _ _ (Node "Bool" [Leaf b]) _
  | Just b' <- safeRead b = return $ Dynamic tbool $ bool b'
  | otherwise = Left $ "Bad bool literal " ++ b

deserialise _ self (Node "AndB" [e1, e2]) env = do
  bb1@(Dynamic t1 _) <- self e1 env
  b1 <- maybeToEither ("Invalid type of argument, expected Bool, found " ++ show t1) $ asBool bb1
  bb2@(Dynamic t2 _) <- self e2 env
  b2 <- maybeToEither ("Invalid type of argument, expected Bool, found " ++ show t2) $ asBool bb2
  return $ Dynamic tbool $ andb b1 b2
deserialise _ _ (Node "AndB" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise _ self (Node "OrrB" [e1,e2]) env = do
  bb1@(Dynamic t1 _) <- self e1 env
  b1 <- maybeToEither ("invalid type of argument, expected bool, found " ++ show t1) $ asBool bb1
  bb2@(Dynamic t2 _) <- self e2 env
  b2 <- maybeToEither ("Invalid type of argument, expected Bool, found " ++ show t2) $ asBool bb2
  return $ Dynamic tbool $ andb b1 b2
deserialise _ _ (Node "OrrB" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise _ self (Node "NegB" [e1]) env = do
  bb1@(Dynamic t1 _) <- self e1 env
  b1 <- maybeToEither ("invalid type of argument, expected bool, found " ++ show t1) $ asBool bb1
  return $ Dynamic tbool $ negb b1
deserialise _ _ (Node "NegB" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise old self e env = old self e env
