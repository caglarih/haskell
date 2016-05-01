main :: IO()
main = print $ maximum [x*y | x <- [999,998..100], y <- [999,998..100], let s = show (x*y), s == (reverse s)]
