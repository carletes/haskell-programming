module Exercise_applyTimes where

applyTimes :: (Eq n, Num n) => n -> (a -> a) -> a -> a
applyTimes 0 f a = a
applyTimes n f a = f $ applyTimes (n - 1) f a

-- applyTimes 5 (+1) 5 =>
-- (+1) $ applyTimes (5 - 1) (+1) 5 =>
-- (+1) $ applyTimes 4 (+1) 5 =>
-- (+1) $ (+1) $ applyTimes (4 - 1) (+1) 5 =>
-- (+1) $ (+1) $ applyTimes (3) (+1) 5 =>
-- (+1) $ (+1) $ (+1) $ applyTimes (3 - 1) (+1) 5 =>
-- (+1) $ (+1) $ (+1) $ applyTimes 2 (+1) 5 =>
-- (+1) $ (+1) $ (+1) $ (+1) $ applyTimes (2 - 1) (+1) 5 =>
-- (+1) $ (+1) $ (+1) $ (+1) $ applyTimes 1 (+1) 5 =>
-- (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ applyTimes (1 - 1) (+1) 5 =>
-- (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ applyTimes 0 (+1) 5 =>
-- (+1) $ (+1) $ (+1) $ (+1) $ (+1) $ 5 =>
-- (+1) $ (+1) $ (+1) $ (+1) $ 6 =>
-- (+1) $ (+1) $ (+1) $ 7 =>
-- (+1) $ (+1) $ 8 =>
-- (+1) $ 9
-- 10
