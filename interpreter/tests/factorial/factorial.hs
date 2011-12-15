module Main where

main = do 
    let n = 10
    putStrLn $ "Factorial 10 is: " ++ (show $ fac n)

fac :: Int -> Int
fac 0 = 1
fac n = n*fac (n-1)

