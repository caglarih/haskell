main::IO()
main = print ( sum [x | x <- takeWhile (<=1000000) fibs, (mod x 2)==0 ])
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
