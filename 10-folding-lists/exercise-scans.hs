fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 fibs

fibsBellow100 = takeWhile (< 100) fibs

factorials = scanl (*) 1 [2..]
