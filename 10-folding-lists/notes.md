# Chapter 10: Folding lists

Introduces the concept
of [catamorphisms](https://en.wikipedia.org/wiki/Catamorphism)
(functions that decompose structure), of which folds are examples.


## Right fold

The "right" bit in `foldr` comes from the fact that it is
_right-associative_:

    foldr :: (a -> b -> b) -> b -> [a] -> b
	foldr f z []     = z
	foldr f z (x:xs) = f x (foldr f z xs)

In this implementation of `foldr`, the initial value is called `z` to
denote that it is the natural zero-element for `f`, so that:

    sum     = foldr (+) 0
	product = foldr (*) 1
	concat  = foldr (++) []

Associating to the right means that `foldr` first *traverses* the
list, and then it *folds* it (that is, it evaluated the folding
function):

    foldr (+) 0 [1..3] = (1 + (2 + (3 + 0)))

### Right fold and evaluation

Given that:

	foldr f z (x:xs) = f x (foldr f z xs)

It follows that, if a given `f` does *not* necessarily evaluate its
second argument, then `foldr` *may* be applied to infinite
lists. Example:

    any :: (a -> Bool) -> [a] -> Bool
	any f xs = foldr (\x b -> f x || b) False xs

In this case, the function `\x b -> f x || b` does not evaluate `b` if
`f x` is `True` (because `(||)` does not evaluate `b` if `f x` is
`True`). So this works:

    λ> let any f xs = foldr (\x b -> f x || b) False xs
    λ> any even [1..]
    True

However, this will not finish:

    λ> any even (repeat 1)

A simpler example, perhaps. Give:

    const :: a -> b -> a
	const a _ = a

Then:

    λ> foldr const 42 [..]
	42


## Left fold

The left fold is defined like this:

    foldl :: (b -> a -> b) -> b -> [a] -> b
	foldl f acc []     = acc
	foldl f acc (x:xs) = foldl f (f acc x) xs

It seems that initial element for `foldl` is called an accumulator,
instead of a zero.

Note that the parameters for the folding function `f` are reversed in
respect to `foldr`.

Unlike `foldr`, it it left-associaive, in the sense that:

    foldl (+) 0 [1..3] = (((0 + 1) + 2) + 3)
    foldr (+) 0 [1..3] = (1 + (2 + (3 + 0)))

Another difference with `foldr` is that `foldl` unconditionally
evaluates the spine of the list, so it cannot be used with infinite
lists.

Moreover, since `foldl` first evaluates the spine, when used with very
long lists it will create many thunks (one for each list element). In
order to avoid the creation of so many thunks, use `foldf'`
