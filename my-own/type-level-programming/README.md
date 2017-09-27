# Type-level programming

This is a [Literate Markdown][] text. It can be run with `stack` from
the top-level directory of this repository:

```
$ stack repl type-level-programming
type-level-programming-0.0.0: initial-build-steps (lib)
Configuring GHCi with the following packages: type-level-programming
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/carlos/.ghci
[1 of 1] Compiling README           ( /home/carlos/src/haskell-programming/my-own/type-level-programming/README.lhs, interpreted )
Ok, modules loaded: README.
Loaded GHCi configuration from /tmp/ghci22537/ghci-script
*README README>
```

We need a bit of Haskell boilerplate first:

```haskell
module README where
```

## Introduction

I started with Matt Parson's [Basic Type Level Programming in Haskell][].

Some examples of basic data types follow:

```haskell
data Unit = MkUnit

data MyBool = True | False
```

In this example `Unit` and `MyBool` are _type constructors_ (which
define types), whereas `MkUnit`, `True` and `False` are _data
constructors_ (which define values).

Both `Unit` and `MyBool` are _concrete types_, since their kind is
`*`:

    λ :k Unit
    Unit :: *
    λ :k MyBool
    MyBool :: *
    λ

A more complex type is introduced:

```haskell
data IntAndChar = MkIntAndChar Int Char
```

The data constructor `MkIntChar` accepts two arguments, but the type
`IntAndChar` is still a concrete type:

    λ :k IntAndChar
    IntAndChar :: *
    λ

[Basic Type Level Programming in Haskell]: http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
[Literate Markdown]: https://github.com/sol/markdown-unlit
