{-# LANGUAGE GADTs, RankNTypes #-}
module Vyom.Data.Pretty where

newtype Pretty h a = Pretty { unPretty :: Int -> String }

-- UTILITY
-- Basic ops for Pretty
sopString :: String -> Pretty h a
sopString = Pretty . const

sop0 :: Show a => a -> Pretty h a
sop0 = sopString . show

sop1 :: String -> Pretty h a -> Pretty h b
sop1 op a = Pretty $ \i -> "(" ++ op ++ " " ++ unPretty a i ++ ")"

sop2 :: String -> Pretty h a -> Pretty h b -> Pretty h c
sop2 op a b = Pretty $ \i -> "(" ++ unPretty a i ++ " " ++ op ++ " " ++ unPretty b i ++ ")"

sop3 :: String -> String -> String -> Pretty h a -> Pretty h b -> Pretty h c -> Pretty h d
sop3 o1 o2 o3 a b c = Pretty $ \i -> "(" ++ o1 ++ " " ++ unPretty a i ++ " " ++ o2 ++ " " ++ unPretty b i ++ " " ++ o3 ++ " " ++ unPretty c i ++ ")"

-- Prettyprint something
pretty :: Pretty h a -> String
pretty e = unPretty e 0
