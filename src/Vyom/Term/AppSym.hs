{-# LANGUAGE RankNTypes #-}
module Vyom.Term.AppSym where

import Vyom

-- Functions that can be applied
class AppSym r where
  app :: r h (a -> b) -> r h a -> r h b

instance AppSym Run where
  app = rop2 id

instance AppSym Pretty where
  app = sop2 ""

instance AppSym Expr where
  app = eop2 "App"


deserialise :: AppSym r => ExtensibleDeserialiser r
deserialise _ self (Node "App" [e1, e2]) env = do
  Dynamic t1 d1 <- self e1 env
  Dynamic t2 d2 <- self e2 env
  AsArrow _ arrCast <- return $ unTypQ t1
  let errarr = fail $ "operator type is not an arrow: " ++ show t1
  (ta,tb,equT1ab) <- maybe errarr return arrCast
  let df = eqCast equT1ab d1
  case gcast t2 ta d2 of
    Just da -> return . Dynamic tb $ app df da
    _ -> fail $ unwords ["Bad types of the application:", show t1, " and ", show t2]
deserialise _ _ (Node "App" es) _ = Left $ "Invalid number of arguments, expected 2, found " ++ show (length es)

deserialise old self e env = old self e env
