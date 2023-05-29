module Vyom.Term.ListSym where

import Data.Kind (Type)

import Vyom
import Vyom.Data.TypeRep
import Util (maybeToEither)

import qualified Codec.Serialise as S

-- Lists
class ListSym r where
  isEmpty :: r h [a] -> r h Bool
  -- Need to pass the type of elements, because we don't support polymorphism
  -- TODO: Implement rank1 types a.k.a. polymorphism
  empty :: TypeRep a -> r h [a]
  -- The rest don't need type args
  cons :: r h a -> r h [a] -> r h [a]
  -- TODO: Make total
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
  empty t = eopList "Empty" [serialiseTypeRep t]
  cons = eop2 "Cons"
  car = eop1 "Car"
  cdr = eop1 "Cdr"

deserialise :: ListSym r => ExtensibleDeserialiser r
deserialise _ self (Node "IsEmpty" [e]) env = do
  Dyn t d <- self e env
  case t of
    App tc ta -> do
      case (typeRep @Type `eqTypeRep` typeRepKind t, tc `eqTypeRep` typeRep @[]) of
        (Just HRefl, Just HRefl) -> do
          return $ Dyn typeRep (isEmpty d)
        _ -> Left $ "Expected type: [" ++ show ta ++ "]. Found type: " ++ show t
    _ -> Left $ "Expected type: [a]. Found type: " ++ show t
deserialise _ _ (Node "IsEmpty" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ _ (Node "Empty" [t]) _ = do
  SomeTypeRep ta <- decode t
  case typeRep @Type `eqTypeRep` typeRepKind ta of
    Just HRefl -> return $ Dyn (App (typeRep @[]) ta) (empty ta)
deserialise _ _ (Node "Empty" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ self (Node "Cons" [ee, el]) env = do
  Dyn te de <- self ee env
  Dyn tl dl <- self el env
  case (App (typeRep @[]) te) `eqTypeRep` tl of
    Just HRefl -> return $ Dyn tl (cons de dl)
    _ -> Left $ "Expected type: [" ++ show te ++ "]. Found type: " ++ show tl
deserialise _ _ (Node "Cons" es) _ = Left $ "Invalid number of arguments, expected 3, found " ++ show (length es)

deserialise _ self (Node "Car" [el]) env = do
  Dyn t d <- self el env
  case t of
    App tc ta -> do
      case (typeRep @Type `eqTypeRep` typeRepKind t, tc `eqTypeRep` typeRep @[]) of
        (Just HRefl, Just HRefl) -> return $ Dyn ta (car d)
        _ -> Left $ "Expected type: [" ++ show ta ++ "]. Found type: " ++ show t
    _ -> Left $ "Expected type: [a]. Found type: " ++ show t
deserialise _ _ (Node "Car" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise _ self (Node "Cdr" [e]) env = do
  Dyn t d <- self e env
  case t of
    App tc ta -> do
      case (typeRep @Type `eqTypeRep` typeRepKind t, tc `eqTypeRep` typeRep @[]) of
        (Just HRefl, Just HRefl) -> return $ Dyn (App (typeRep @[]) ta) (cdr d)
        _ -> Left $ "Expected type: [" ++ show ta ++ "]. Found type: " ++ show t
    _ -> Left $ "Expected type: [a]. Found type: " ++ show t
deserialise _ _ (Node "Cdr" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise old self e env = old self e env
