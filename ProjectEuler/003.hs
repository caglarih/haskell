primes :: [Integer]
primes = sieve [2..] where
  sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

factors :: Integer -> [Integer]
factors = factIter primes
  where
    factIter :: [Integer] -> Integer -> [Integer]
    factIter _ 1 = []
    factIter (p:ps) x
      | p * p > x    = [x]
      | mod x p == 0 = p : factIter (p:ps) (div x p)
      | otherwise    = factIter ps x

main :: IO()
main = print $ maximum $ factors 600851475143
