# Chapter 11: Algebraic datatypes

The first sections of this chapter are structured around this informal
definition of algebraic datatypes:

> An algebraic datatype is an enumeration of constructors that have
> zero or more arguments.

Constructors can be _type constructors_ (those to the left of the
equal sign) or __data constructors_ (those to the right):

    data Maybe a = Nothing | Just a

    <-- type -->   <---- data ---->

When they take no arguments (like `Nothing`, in the example above),
constructors are called _constants_.

Data constructors accepting parameters are, in a way, like functions:

    λ :t Just
    Just :: a -> Maybe a
    

Type and data constructors live in different spaces: Type constructors
are _compile-time_ artifacts, whereas data constructors are _run-time
artifacts_. That's why the following does not make sense:

    λ :t Maybe

    <interactive>:1:1: error:
        • Data constructor not in scope: Maybe
        • Perhaps you meant variable ‘maybe’ (imported from Prelude)
    

A similar concept exists for types: _kinds_:

    λ :k Maybe
    Maybe :: * -> *
    


## `newtype`

The keyword `newtype` is used to define types with a single unary data
constructor:

    newtype Goats = Goats Int deriving (Eq, Show)
	newtype Cows = Cows Int deriving (Eq, Show)

Types defined with `newtype` do not have any extra run-time overhead
compared to the types they are built upon --- that is, `Goats 42` is
the same thing as `Int 32`.

At compile-time, though, types defined with `newtype` are distinct
from those thet are built upon. If we have:

    tooManyGoats :: Goats -> Bool
	tooManyGoats (Goats n) = n > 42

then the following will not compile:

     tooManyGoats 43

    <interactive>:16:14: error:
        • No instance for (Num Goats) arising from the literal ‘43’
        • In the first argument of ‘tooManyGoats’, namely ‘43’
          In the expression: tooManyGoats 43
          In an equation for ‘it’: it = tooManyGoats 43
    
