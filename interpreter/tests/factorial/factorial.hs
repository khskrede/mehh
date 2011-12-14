module Main where

main = do 
    let n = 10
    putStrLn $ "Factorial 10 is: " ++ (show $ fib n)

fib :: Integer -> Integer
fib 0 = 1
fib n = n*fib (n-1)

