module Vyom.Data.TypeRep where

import qualified Codec.Serialise as S (serialise, deserialiseOrFail)
import qualified Data.ByteString.Lazy.Char8 as C8 (pack, unpack)
import Type.Reflection (TypeRep, SomeTypeRep(..))

import Vyom.Data.Exp (Serialise(..), ExprU(..))

serialiseTypeRep :: TypeRep a -> ExprU
serialiseTypeRep rep = Node "TypeRep"
  [Leaf (C8.unpack (S.serialise (SomeTypeRep rep)))]

instance Serialise SomeTypeRep where
  encode rep = Node "TypeRep"
    [Leaf (C8.unpack (S.serialise rep))]
  decode (Node "TypeRep" [Leaf s]) = case S.deserialiseOrFail (C8.pack s) of
    Left _ -> Left "Expected TypeRep"
    Right rep -> return rep
