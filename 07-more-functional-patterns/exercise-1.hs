-- All of these declarations are equivalent.

mTh_a x y z = x * y * z

mTh_b x y = \z -> x * y * z

mTh_c x = \y -> \z -> x * y * z

mTh_d = \x -> \y -> \z -> x * y * z
