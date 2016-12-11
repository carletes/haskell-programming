# Chapter 14: Monoid, semigroup

A _monoid_ is a binary, associative operation with an identity
element:

    class Monoid m where
	  mappend :: m -> m -> m
	  mempty :: m

	  -- `mconcat` is derived from `mappend` and `mempty` by default.
	  mconcat :: [m] -> m
	  mconcat = foldr mappend mempty

      -- Laws:
	  --
	  --     a `mappend` (b `mappend` c) = (a `mappend` b) `mappend` c
      --     a `mappend` mempty = mempty `mappend` a = a
	  --     mconcat = foldr mappend mempty
