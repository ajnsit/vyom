{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}

module Vyom.Data.Dyn where

import Type.Reflection
import Data.Type.Equality

import GHC.Base
import GHC.Show
import GHC.Exception

data Dyn t = forall a. Dyn (TypeRep a) (t a)

instance Show (Dyn t) where
   -- the instance just prints the type representation.
   showsPrec _ (Dyn t _) =
          showString "<<" .
          showsPrec 0 t   .
          showString ">>"

toDyn :: Typeable a => t a -> Dyn t
toDyn v = Dyn typeRep v

fromDyn :: forall t a. Typeable a => Dyn t -> Maybe (t a)
fromDyn (Dyn t v)
  | Just HRefl <- t `eqTypeRep` (typeRep @a) = Just v
  | otherwise = Nothing

dynTypeRep :: Dyn t -> SomeTypeRep
dynTypeRep (Dyn tr _) = SomeTypeRep tr

