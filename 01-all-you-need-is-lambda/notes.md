Chapter 1: All You Need is Lambda
=================================

An introduction to lambda calculus.

On domains, codomains and ranges: Given the function:

    f(1) = A
	f(2) = A
	f(3) = B

then:

    domain(f) = {1, 2, 3}
	codomain(f) = {A, B}
	range(f) = {A, A, B}


1.4 The structure of lambda terms
---------------------------------

An _abstraction_ is a function. It consists of a _head_, and a _body_.

In the expression:

    \x.x

we have:

* A _head_ (`\x.`), formed by _lambda_ (`\`) and an _parameter_ (`x`)
* A _body_ (`x`)

The variable `x` in the body is a _bound variable_, which will be
substituted by the same value when the function `\x.x` is _applied_.

Variables in the body of a function which are not bound to variables
in the head are called _free variables_.

In the expression:

    \x.xy

the variable `y` is free.

Note that heads only accept *one* parameter. Expressions like:

    \xy.xy

are just short-hand for:

    \x.(\y.xy)


1.8 Combinators
---------------

A _combinator_ is a lambda term with no free variables.
