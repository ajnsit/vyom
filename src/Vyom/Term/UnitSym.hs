module Vyom.Term.UnitSym where

import Data.Kind (Type)

import Vyom
import Util (safeRead)

class UnitSym r where
  unit :: r h ()

instance UnitSym Run where
  unit = rop0 ()

instance UnitSym Pretty where
  unit = sop0 ()

instance UnitSym Expr where
  unit = eop0 "()" ()


deserialise :: UnitSym r => ExtensibleDeserialiser r
deserialise _ _ (Node "()" [Leaf s]) _
  | Just "()" <- safeRead s = return $ Dyn (typeRep @()) unit
  | otherwise = Left $ "Bad unit literal " ++ s
deserialise _ _ (Node "()" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise old self e env = old self e env
