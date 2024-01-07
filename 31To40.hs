-- Problem 31
isPrime :: Int -> Bool
isPrime 1 = True
isPrime n = helper n (n-1)
    where 
        helper n 1 = True
        helper n i = mod n i /= 0 && helper n (i-1)

-- Problem 32
myGCD :: Int -> Int -> Int
myGCD 0 n = n
myGCD x y 
    | x < 0     = myGCD (-x) y
    | y < 0     = myGCD x (-y)
    | x < y     = helper y x
    | otherwise = helper x y
    where
        helper :: Int -> Int -> Int
        helper x 0 = x
        helper 0 x = x
        helper x y = helper y (x `mod` y)

-- Problem 33