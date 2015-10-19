{-# LANGUAGE InstanceSigs #-}
module Vyom.Term.TupleSym where

import Vyom

-- Literal values
class TupleSym r where
  tuple :: r h a -> r h b -> r h (a,b)
  first :: r h (a,b) -> r h a
  second :: r h (a,b) -> r h b
  -- TODO: Add more tuple ops

instance TupleSym Run where
  tuple = rop2 (,)
  first = rop1 fst
  second = rop1 snd

instance TupleSym Pretty where
  tuple = sop2 ","
  first = sop1 "first"
  second = sop1 "second"

instance TupleSym Expr where
  tuple = eop2 "Tuple"
  first = eop1 "First"
  second = eop1 "Second"

deserialise :: TupleSym r => ExtensibleDeserialiser r
deserialise _ self (Node "Tuple" [e1, e2]) env = do
  Dynamic t1 d1 <- self e1 env
  Dynamic t2 d2 <- self e2 env
  return $ Dynamic (ttuple t1 t2) (tuple d1 d2)
deserialise _ _ (Node "Tuple" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise _ self (Node "First" [e]) env = do
  Dynamic t d <- self e env
  AsTuple _ arrCast <- return $ unTypQ t
  let errarr = fail $ "operator type is not a tuple: " ++ show t
  (ta,_,equTab) <- maybe errarr return arrCast
  let df = eqCast equTab d
  return $ Dynamic ta $ first df
deserialise _ _ (Node "First" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise _ self (Node "Second" [e]) env = do
  Dynamic t d <- self e env
  AsTuple _ arrCast <- return $ unTypQ t
  let errarr = fail $ "operator type is not a tuple: " ++ show t
  (_,tb,equTab) <- maybe errarr return arrCast
  let df = eqCast equTab d
  return $ Dynamic tb $ second df
deserialise _ _ (Node "Second" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise old self e env = old self e env
