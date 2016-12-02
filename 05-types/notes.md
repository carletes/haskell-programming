Chapter 5: Types
================

Functions are indeed values, too, and the `(->)` operator is their
type constructor:

	λ> :i (->)
	data (->) t1 t2
	infixr 0 `(->)`

Note the right associativity, which means that:

    addStuff :: a -> a -> a

is in fact:

    addStuff :: a -> (a -> a)


Type-checking incomplete definitions
------------------------------------

GHCi can give you type signatures of incomplete definitions (might be
usueful for scaffolding code!):

	λ> let f :: a -> a -> a -> a ; f = undefined
	λ> :t f
	f :: a -> a -> a -> a
	λ>


5.6 Parametric polymorphism
---------------------------

Related to
[Theorem's for Free](https://www.mpi-sws.org/~dreyer/tor/papers/wadler.pdf):

> All you can really do with a parametrically polymorphic value is
> pass or not pass it to some other expression.
