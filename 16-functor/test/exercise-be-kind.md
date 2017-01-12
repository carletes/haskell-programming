# Exercise: Be Kind

Given a type signature, determine the kinds of each type variable:

> What’s the kind of `a`?
>
>     a -> a

`a` is not applied to any other type, so its kind is `*` (i.e., it's a
_type constant_).

> What are the kinds of `b` and `T` ? (The `T` is capitalized on purpose!)
>
>     a -> b a -> T (b a)

`b` is applied to one type (`a`), so its kind is `* -> *`.

As for `T`, since it's capitalized, I assume it's a concrete type
(defined somewhere else) which is applied to one type (which is
`(b a)`), so its kind is also `* -> *`.

> What’s the kind of `c`?
>
>     c a b -> c b a

`c` is applied to two types (`a b`), so its kind is `* -> * -> *`.
