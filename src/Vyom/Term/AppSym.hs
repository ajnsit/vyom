{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Vyom.Term.AppSym where

import Vyom

import GHC.Base


-- Functions that can be applied
class AppSym r where
  app :: r h (a -> b) -> r h a -> r h b

-- Helpful synonyms
infixl 9 #$
(#$) :: AppSym r => r h (a -> b) -> r h a -> r h b
f #$ a = app f a

instance AppSym Run where
  app = rop2 id

instance AppSym Pretty where
  app = sop2 ""

instance AppSym Expr where
  app = eop2 "App"

deserialise :: AppSym r => ExtensibleDeserialiser r
deserialise _ self (Node "App" [e1, e2]) env = do
  res <- self e1 env
  da@(Dyn ta va) <- self e2 env
  case res of
    df@(Dyn (Fun targ tres) vf) -> do
      -- mkApp targ tres vf ta va
      case (targ `eqTypeRep` ta, typeRep @Type `eqTypeRep` typeRepKind tres) of
        (Just HRefl, Just HRefl) -> do
          return $ Dyn tres $ app vf va
        _ -> Left $ "Mismatched type of function argument. Expected " <> show targ <> ", found " <> show ta
    (Dyn tactual _) -> Left $ "Type Error. Expected a function with arg " <> show ta <> ", found " <> show tactual
deserialise _ _ (Node "App" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise old self e env = old self e env

