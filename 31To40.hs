import Data.List (group)
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
coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1

-- Problem 34
totient :: Int -> Int
totient 1 = 1
totient x = helper [1..x] x
    where
        helper [] _     = 0
        helper (x:xs) n = if coprime x n then 1 + helper xs n else 0 + helper xs n

totient2 :: Int -> Int
totient2 n = length [x | x <- [1..n], coprime x n]

-- Problem 35
primeFactors :: Int -> [Int]
primeFactors n = helper n 2
    where
        helper 1 _ = []
        helper n f
            | n `mod` f == 0 = f : helper (n `div` f) f
            | otherwise      = helper n (f+1)

-- Problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map helper . group . primeFactors
    where
        helper xs = (head xs, length xs)

-- Problem 37
totientImproved :: Int -> Int
totientImproved m = product [ p - 1 * p ^ (c-1) | (p, c) <- primeFactorsMult m]

-- Problem 38
primesR :: Int -> Int -> [Int]
primesR i j = [ x | x <- [i..j], isPrime x]

-- Problem 40
goldbach :: Int -> (Int, Int)
goldbach n = head [ (x,y) | x <- primes, y <- primes, x + y == n ]
    where
        primes = primesR 2 n

-- Problem 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList i j = [ (x,y) | (x,y) <- [ goldbach a | a <- [i .. j], even a] ]

goldbachListMinPrime :: Int -> Int -> Int -> [(Int, Int)]
goldbachListMinPrime i j m = take 4 [ (x,y) | (x,y) <- goldbachList i j, x > m && y > m ]

