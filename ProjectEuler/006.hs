main :: IO()
main = print $ (sum [1..100])^2 - sum [x*x|x<-[1..100]]
