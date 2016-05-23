main:: IO()
main = print $ foldr lcm_ 1 [1..20]

lcm_ :: Integer -> Integer -> Integer
lcm_ x y = div (x*y) (gcd_ x y)

gcd_ :: Integer -> Integer -> Integer
gcd_ x y = if y == 0 then x else gcd_ y (mod x y)
