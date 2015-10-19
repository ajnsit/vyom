{-# LANGUAGE InstanceSigs #-}
module Vyom.Term.ListSym where

import Vyom
import Util (maybeToEither)

-- Lists
class ListSym r where
  isEmpty :: r h [a] -> r h Bool
  -- Need to pass the type of elements, because we don't support polymorphism
  -- TODO: Implement rank1 types a.k.a. polymorphism
  empty :: TypQ a -> r h [a]
  -- The rest don't need type args
  cons :: r h a -> r h [a] -> r h [a]
  car :: r h [a] -> r h a
  cdr :: r h [a] -> r h [a]

instance ListSym Run where
  isEmpty = rop1 null
  empty _ = rop0 []
  cons = rop2 (:)
  car = rop1 head
  cdr = rop1 tail

instance ListSym Pretty where
  isEmpty = sop1 "null"
  empty _ = sopString "[]"
  cons = sop2 ":"
  car = sop1 "car"
  cdr = sop1 "cdr"

instance ListSym Expr where
  isEmpty = eop1 "IsEmpty"
  empty t = eopList "Empty" [typToExp (Typ t)]
  cons = eop2 "Cons"
  car = eop1 "Car"
  cdr = eop1 "Cdr"


deserialise :: ListSym r => ExtensibleDeserialiser r
deserialise _ self (Node "IsEmpty" [e]) env = do
  Dynamic t d <- self e env
  AsList _ arrCast <- return $ unTypQ t
  let errarr = fail $ "type is not a list: " ++ show t
  (_, equTab) <- maybe errarr return arrCast
  let df = eqCast equTab d
  return $ Dynamic tbool (isEmpty df)
deserialise _ _ (Node "IsEmpty" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ _ (Node "Empty" [t]) _ = do
  Typ ta <- expToTyp t
  return $ Dynamic (tlist ta) (empty ta)
deserialise _ _ (Node "Empty" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ self (Node "Cons" [ee, el]) env = do
  Dynamic te de <- self ee env
  Dynamic cdr dl <- self el env
  AsList _ arrCast <- return $ unTypQ cdr
  (ta, equTab) <- maybeToEither ("type is not a list: " ++ show cdr) arrCast
  eqae <- maybeToEither ("Expected type: " ++ show te ++ " found type: " ++ show ta) $ eqT ta te
  let dlf = eqCast (trans equTab (eqTrans1 eqae)) dl
  return $ Dynamic (tlist te) (cons de dlf)
deserialise _ _ (Node "Cons" es) _ = Left $ "Invalid number of arguments, expected 3, found " ++ show (length es)

deserialise _ self (Node "Car" [el]) env = do
  Dynamic cdr dl <- self el env
  AsList _ arrCast <- return $ unTypQ cdr
  (ta, equTab) <- maybeToEither ("type is not a list: " ++ show cdr) arrCast
  let dlf = eqCast equTab dl
  return $ Dynamic ta (car dlf)
deserialise _ _ (Node "Car" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ self (Node "Cdr" [e]) env = do
  Dynamic t d <- self e env
  AsList _ arrCast <- return $ unTypQ t
  (ta, equTab) <- maybeToEither ("type is not a list: " ++ show t) arrCast
  let df = eqCast equTab d
  return $ Dynamic (tlist ta) (cdr df)
deserialise _ _ (Node "Cdr" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise old self e env = old self e env
