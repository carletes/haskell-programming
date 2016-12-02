applyTimes :: (Eq n, Num n) => n -> (a -> a) -> a -> a
applyTimes 0 f a = a
applyTimes n f a = f $ applyTimes (n - 1) f a
