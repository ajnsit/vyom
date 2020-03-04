{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Vyom.Term.TupleSym where

import Data.Kind (Type)

import Vyom

-- Literal values
class TupleSym r where
  tuple :: r h a -> r h b -> r h (a,b)
  first :: r h (a,b) -> r h a
  second :: r h (a,b) -> r h b

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
  Dyn t1 d1 <- self e1 env
  Dyn t2 d2 <- self e2 env
  return $ Dyn (App (App (typeRep @(,)) t1) t2) (tuple d1 d2)
deserialise _ _ (Node "Tuple" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise _ self (Node "First" [e]) env = do
  Dyn t d <- self e env
  case t of
    App (App tc ta) tb -> do
      case (typeRep @Type `eqTypeRep` typeRepKind t, tc `eqTypeRep` typeRep @(,)) of
        (Just HRefl, Just HRefl) -> return $ Dyn ta (first d)
        _ -> Left $ "Expected type: (" ++ show ta ++ "," ++ show tb ++ "). Found type: " ++ show t
    _ -> Left $ "Expected type: (a,b). Found type: " ++ show t
deserialise _ _ (Node "First" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise _ self (Node "Second" [e]) env = do
  Dyn t d <- self e env
  case t of
    App (App tc ta) tb -> do
      case (typeRep @Type `eqTypeRep` typeRepKind t, tc `eqTypeRep` typeRep @(,)) of
        (Just HRefl, Just HRefl) -> return $ Dyn tb (second d)
        _ -> Left $ "Expected type: (" ++ show ta ++ "," ++ show tb ++ "). Found type: " ++ show t
    _ -> Left $ "Expected type: (a,b). Found type: " ++ show t
deserialise _ _ (Node "Second" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise old self e env = old self e env

