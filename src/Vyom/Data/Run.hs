{-# LANGUAGE GADTs, RankNTypes #-}
module Vyom.Data.Run where

-- Runtime representation for values -
--   runtime environment (h) -> the actual value (a)
newtype Run h a = Run { run :: h -> a }

-- UTILITY
-- Basic ops for Run
rop0 :: a -> Run h a
rop0 = Run . const

rop1 :: (a -> b) -> Run h a -> Run h b
rop1 f a = Run $ \h -> f (run a h)

rop2 :: (a -> b -> c) -> Run h a -> Run h b -> Run h c
rop2 f a b = Run $ \h -> f (run a h) (run b h)

rop3 :: (a -> b -> c -> d) -> Run h a -> Run h b -> Run h c -> Run h d
rop3 f a b c = Run $ \h -> f (run a h) (run b h) (run c h)

