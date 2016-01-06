{-# LANGUAGE InstanceSigs #-}
module Vyom.Term.CharSym where

import Vyom
import Util (safeRead)

-- Literal values
class CharSym r where
  char :: Char -> r h Char
  -- TODO: Add more char ops

instance CharSym Run where
  char = rop0

instance CharSym Pretty where
  char = sop0

instance CharSym Expr where
  char = eop0 "Char"


deserialise :: CharSym r => ExtensibleDeserialiser r
deserialise _ _ (Node "Char" [Leaf s]) _
  | Just s' <- safeRead s = return $ Dynamic tchar $ char s'
  | otherwise = Left $ "Bad char literal " ++ s
deserialise _ _ (Node "Char" es) _ = Left $ "Invalid number of arguments, expected 1, found " ++ show (length es)

deserialise old self e env = old self e env
