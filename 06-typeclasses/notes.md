Chapte 6: Typeclasses
=====================

An interesting remark at the end of 6.13. In the follwoing definition:

    f :: Int -> Int -> Int

the function `f` may do a lot of things (including, potentially,
dangeroues things), because `Int` is an instance of many classes. If
`f` needed only a subset of what `Int` provides, it would be better to
declare it like this:

    f :: (Enum a) => a -> a -> a

This new `f` would have access to fewer functionality, and thus would
be potentially sager (and could be applied to more types, too).
