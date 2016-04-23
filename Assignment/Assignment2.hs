--
--Ihsan Burak Caglar
--150120057
--Blg458E - Functional Programming
--Assignment 2
--
module Assignment2 where
import           Prelude

isPrime :: Integer -> Bool
isPrime n
 | n<2       = False
 | otherwise = not $ foldr (\x r -> ((||r) . (\b -> n `mod` b == 0)) x ) False [2..(n - 1)]

goldbach :: Integer -> (Integer, Integer)
goldbach n
 | even n && n>4 = head [(t,n-t) | t <- [2 .. n],isPrime t, isPrime $ n-t, t <= (n-t)]
 | otherwise     = (0,0)

goldbachList :: Integer -> Integer -> [(Integer,Integer)]
goldbachList a b = [ goldbach t | t <- [a..b], even t ]

goldbachList' :: Integer -> Integer -> Integer -> [(Integer,Integer)]
goldbachList' a b c = filter (\(x,y) -> x > c && y > c) $ goldbachList a b

beginWith :: Int -> [String] -> [String]
beginWith a = map ((head.show)a:)

--problem 49
gray :: Int -> [String]
gray 0 = [""]
gray n = (beginWith 0 . gray) (n-1) ++ (beginWith 1 . reverse . gray $n-1)
