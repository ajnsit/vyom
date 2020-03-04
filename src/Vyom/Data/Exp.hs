{-# LANGUAGE DeriveGeneric #-}
module Vyom.Data.Exp where

import Data.Binary
import GHC.Generics (Generic)
import Vyom.Data.ErrorOr (ErrorOr)

-- Extensible Untyped Serialisation format
data ExprU
  = Leaf String
  | Node String [ExprU]
  deriving (Eq, Read, Show, Generic)
instance Binary ExprU

-- Things which can be encoded/decoded from ExprU
class Serialise a where
  encode :: a -> ExprU
  decode :: ExprU -> ErrorOr a

-- Extensible Typed Serialisation format
newtype Expr h a = Expr { serialise :: ExprU }

instance Serialise (Expr h a) where
  encode = serialise
  decode = Right . Expr

-- UTILITY
-- Basic ops for Expr
eopString :: String -> String -> Expr h a
eopString tag s = Expr $ Node tag [Leaf s]

eop0 :: Show a => String -> a -> Expr h a
eop0 tag a = eopString tag $ show a

-- Can't use map with serialise because then we lose newtype efficiency
-- So we have different eops for different arities

eop1 :: String -> Expr h a -> Expr h b
eop1 tag a = Expr $ Node tag [serialise a]

eop2 :: String -> Expr h a -> Expr h b -> Expr h c
eop2 tag a b = Expr $ Node tag [serialise a, serialise b]

eop3 :: String -> Expr h a -> Expr h b -> Expr h c -> Expr h d
eop3 tag a b c = Expr $ Node tag [serialise a, serialise b, serialise c]

eopList :: String -> [ExprU] -> Expr h b
eopList tag ls = Expr $ Node tag ls
