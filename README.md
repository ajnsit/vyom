# VYOM

Vyom is an extensible algebra for creating typesafe DSLs in Haskell.

An Algebra is a self contained unit that defines values, and the operations supported by those values. By composing multiple algebras together, it is possible to iteratively extend DSLs. In particular, you *don't* need the source code for an existing DSL to be able to extend it in a completely typesafe manner.

It is also possible to take an object term written in one DSL and run it with another DSL as long as this DSL extends the previous one. This can even be on a remote system, i.e. serialise a compiled language term and send it to another machine, deserialise it and run it in a different DSL.
  
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
5. Conditional (if-then-else)

## TODO

1. The type system is currently work in progress and non-extensible. It does provide enough power to implement all example constructs, but there are plans to make it completely generic.
2. There is no type inference (only type checking). It may be implemented in the future.
3. Currently only Rank0 types are supported. Rank1 types are currently under implementation. Higher ranked types may be supported in the future.
