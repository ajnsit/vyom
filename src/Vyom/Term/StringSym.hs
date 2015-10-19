{-# LANGUAGE InstanceSigs #-}
module Vyom.Term.StringSym where

import Vyom
import Util (safeRead)

-- Literal values
class StringSym r where
  string :: String -> r h String
  -- TODO: Add more string ops

instance StringSym Run where
  string = rop0

instance StringSym Pretty where
  string = sop0

instance StringSym Expr where
  string = eop0 "String"


deserialise :: StringSym r => ExtensibleDeserialiser r
deserialise _ _ (Node "String" [Leaf s]) _
  | Just s' <- safeRead s = return $ Dynamic tstring $ string s'
  | otherwise = Left $ "Bad string literal " ++ s
deserialise _ _ (Node "String" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise old self e env = old self e env
