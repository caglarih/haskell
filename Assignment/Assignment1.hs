{-|
  Ihsan Burak Caglar
  150120057

  BLG458E - Functional Programming
  Assigment 1
  08.03.16
-}

module Assignment1 where

import           Prelude hiding (sqrt)

--problem 15
repli :: [Char] -> Integer -> [Char]
repli _ 0 = []
repli cs n = repliIter (n-1) cs
  where
    repliIter :: Integer -> [Char] -> [Char]
    repliIter 0 cs' = cs'
    repliIter n' cs' = repliIter (n'-1) (cs ++ cs')


--problem 16
dropEvery :: [a] -> Integer -> [a]
dropEvery []     _ = []
dropEvery (a:as) n = reverse(dropStep [a] 1 as)
  where
    dropStep :: [a] -> Integer -> [a] -> [a]
    dropStep acc _ []     = acc
    dropStep acc t (a:as) = if help t then dropStep acc (t+1) as else dropStep (a:acc)(t+1) (as)
      where
        help :: Integer -> Bool
        help t = ((t+1) `mod` (n))== 0

--problem 18
slice :: [a] -> Integer -> Integer -> [a]
slice _ 1 0   = []
slice [] _ _  = []
slice (a:as) str end
  | end < str = []
  | str < 1   = []
  | str == 1  = a : slice as 1 (end - 1)
  | otherwise = slice as (str-1) (end-1)

--works
--rotate2 :: [a] -> Int -> [a]
--rotate2 as 0 = as
--rotate2 (a:as) n
--  | n < 0     = rotate2 (a:as) (length(a:as) + n)
--  | otherwise = rotate2 (as++[a]) (n-1)

--problem 19
rotate :: [a] -> Int -> [a]
rotate as 0 = as
rotate (a:as) n
  | n < 0     = merge (transfer (length(a:as) + n) (a:as,[]))
  | otherwise = merge (transfer n (a:as,[]))
    where
      transfer :: Int -> ([a], [a]) -> ([a], [a])
      transfer n (a:as,as')
        | n > 0     = transfer (n-1) (as, as'++[a])
        | otherwise = (a:as,as')
      merge :: ([a], [a]) -> [a]
      merge (as,as') = as++as'

--the smallest integer that is bigger than sqrt n
--sqrt :: Integer -> Integer
--sqrt n = sqrtIter 2
--  where
--    sqrtIter :: Integer -> Integer
--    sqrtIter x
--      | (x*x) < n = sqrtIter (x+1)
--      | otherwise = x

--problem 31
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | otherwise = primeIter n 2
    where
      primeIter :: Integer -> Integer -> Bool
      primeIter a b
        | a == b = True
        | a > b  = if (mod a b) == 0 then False else primeIter a (b+1)

--problem 40
goldbach :: Integer -> (Integer,Integer)
goldbach n = goldbachIter (2, n-2)
  where
    goldbachIter :: (Integer, Integer) -> (Integer,Integer)
    goldbachIter (a,b)
      | test a b  = (a,b)
      | a>b       = error "wrong input"
      | otherwise = goldbachIter(a+1,b-1)
    test :: Integer -> Integer -> Bool
    test c d = isPrime c && isPrime d
