sumN :: (Integral a) => a -> a
sumN 0 = 0
sumN n = n + sumN (n -1)

sumN' :: (Integral a) => a -> a
sumN' n = go n 0 where
  go :: (Integral a) => a -> a -> a
  go 0 acc = acc
  go n acc = go (n - 1) (acc + 1)
