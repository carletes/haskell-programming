-- The McCarthy 91 function yields `𝑥 − 10` when `𝑥 > 100` and `91` otherwise.
-- The function is recursive.

mcCarthy91 n
    | n > 100 = n - 10
    | otherwise = mcCarthy91 $ mcCarthy91 $ n + 11
