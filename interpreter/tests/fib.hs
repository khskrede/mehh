module Main where

main = do 
    let n = 10
    putStrLn $ "The 10. fibonacci number is: " ++ (show $ fib n)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

