# VYOM

Vyom is an extensible algebra for creating typesafe DSLs in Haskell.

An Algebra is a self contained unit that defines values, and the operations supported by those values. By composing multiple algebras together, it is possible to iteratively extend DSLs. In particular, you *don't* need the source code for an existing DSL to be able to extend it in a completely typesafe manner.

It is also possible to take an object term written in one DSL and run it with another DSL as long as this DSL extends the previous one. This can even be on a remote system, i.e. serialise a compiled language term and send it to another machine, deserialise it and run it in a different DSL.
  
## Sample usage

```haskell
-- Let's define a DSL with functions, ints, and booleans only
type SimpleSym r =
  ( IntSym  r
  , BoolSym r
  , LamSym  r
  , AppSym  r
  )

-- A Sample term
-- ===> sampleFunc 10
sample :: SimpleSym r => r () Bool
sample = func #$ int 10

-- A sample function
-- ===> \x -> x + 5 >= 14
-- Function arguments can be referred to by using v0, v1 etc.
func :: SimpleSym r => r () (Int -> Bool)
func = lambda $ (v0 #+ int 5) #>= (int 14)

-- Pretty print
ghci> pretty sample
((\x0 -> ((x0 + 5) >= 14))  10)
-- Eval
ghci> run sample ()
True
```

We can easily extend this DSL in another file -

```haskell
-- Now let's add conditionals, lists, and recursion (using fix) to SimpleSym
type MySym r =
  ( SimpleSym  r
  , ListSym r
  , FixSym r
  )

-- Now we can write a function that sums up all elements of a [Int]
-- sumInts l = if (isEmpty l) then 0 else (car l + sumInts (cdr l))
-- Recursion takes a CPS'd version of the function as argument
sumInts :: MySym r () ([Int] -> Int)
sumInts = recurse $ lambda $
   cond (isEmpty v0)
     (int 0)
     (car v0 #+ v1 #$ cdr v0)

-- A Sample term
-- sumInts [3,2,1]
mySample :: MySym r => r () Int
mySample = sumInts #$ ints
  where
    ints = cons (int 3) (cons (int 2) (cons (int 1) (empty tint)))

-- Pretty print
ghci> pretty mySample
((fix ((\x1 -> (if (null x1) then 0 else ((car x1) + (x0  (cdr x1)))))))  (3 : (2 : (1 : []))))
-- Eval
ghci> run mySample ()
6
```


## Examples

As examples, Vyom provides the following existing algebras -

1. Lambdas
2. Function application
3. 'Fixpoint' operator for implementing recursion
4. Integers
5. Bools
6. Strings
7. Unit type - ()
8. Tuples
9. Lists
10. Conditional (if-then-else)

Look at the included [sample application](app/) for information on how to use them.

## TODO

1. The type system is currently work in progress and non-extensible. It does provide enough power to implement all example constructs, but there are plans to make it completely generic.
2. There is no type inference (only type checking). It may be implemented in the future.
3. Currently only Rank0 types are supported. Rank1 types are currently under implementation. Higher ranked types may be supported in the future.
