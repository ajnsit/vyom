{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Main where

import Vyom
import DSL

main :: IO ()
main = test tint (sample $ sumInts #$ ints)

-- A simple test
-- take a DSL term
-- eval and print it
-- serialise it to Exp
-- deserialise exp to get the term back
-- eval and print the new term

test :: forall a. Show a => TypQ a -> DSL () a -> IO ()
test typ term = do
  putStrLn $ "Input value: " ++ pretty term
  putStrLn $ "Result: " ++ show (run term ())
  putStrLn $ "Serialising value. Value read after deserialisation should be the same."
  putStrLn $ "New value: " ++ pretty readTerm
  putStrLn $ "New result: " ++ show (run readTerm ())
  where
    readTerm :: DSL () a
    readTerm = roundTrip typ term

-- Test round trip serialise and deserialise
roundTrip :: TypQ a -> DSL () a -> DSL () a
roundTrip typ term = case deserialise typ () (serialise term) of
  Left err -> error err
  Right term' -> term'

-- Sample DSL Term
-- g x = f x x
--   where
--     f x y = x + y
sample :: DSL env Int -> DSL env Int
sample anInt = g #$ anInt
  where
    f :: DSL env (Int -> Int -> Int)
    f = tint #\ tint #\ z #+ s z
    g :: DSL env (Int -> Int)
    g = tint #\ (f #$ z) #$ z

-- A function that sums all elements of a [Int]
-- sumInts l = if (isEmpty l) then 0 else (car l + sumInts (cdr l))
-- We use `fix` for recursion
sumInts :: DSL env ([Int] -> Int)
sumInts = fix typ sumIntsCPS
  where
    typ = tarr (tlist tint) tint
    sumIntsCPS = tlist tint #\
      let ls   = z
          self = s z
      in cond (isEmpty ls) (int 0) (car ls #+ self #$ cdr ls)

-- Sample int list
-- [3,2,1]
ints :: DSL env [Int]
ints = cons (int 3) (cons (int 2) (cons (int 1) (empty tint)))
