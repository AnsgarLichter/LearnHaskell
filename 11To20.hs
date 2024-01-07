import Data.List

-- Problem 11
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [ (length xs + 1, x) | (x:xs) <- group xs]

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified [] = []
encodeModified [x] = [Single x]
encodeModified (x:xs) = help 1 x xs
    where
        help n x []
            | n > 1     = [Multiple n x]
            | otherwise = [Single x]
        help n x (y:ys)
            | x == y    = help (n+1) x ys
            | otherwise = help n x [] ++ help 1 y ys

encodeModified2 :: Eq a => [a] -> [ListItem a]
encodeModified2 xs = [ y | x <- group xs, let y = if length x == 1 then Single (head x) else Multiple (length x) (head x)]

encodeModified3 :: Eq a => [a] -> [ListItem a]
encodeModified3 = map encodeHelper . encode
    where
        encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x

-- Problem 12
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified = concatMap toTupel
    where
        toTupel :: ListItem a -> [a]
        toTupel (Single x) = [x]
        toTupel (Multiple 1 x) = [x]
        toTupel (Multiple n x) = x : toTupel (Multiple (n-1) x)

decodeModified2 :: Eq a => [ListItem a] -> [a]
decodeModified2 [] = []
decodeModified2 ((Single x):xs) = x : decodeModified2 xs
decodeModified2 ((Multiple 1 x):xs) = x : decodeModified2 xs
decodeModified2 ((Multiple n x):xs) = x : decodeModified2 (Multiple (n-1) x:xs)

decodeModified3 :: Eq a => [ListItem a] -> [a]
decodeModified3 = foldl (\x y -> x ++ decodeHelper y) []
    where
        decodeHelper (Single x)     = [x]
        decodeHelper (Multiple n x) = replicate n x

-- Problem 13
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = helper 1 x xs
    where
       -- helper :: Int -> a -> [a] -> [ListItem]
        helper n y []
            | n == 1 = [Single y]
            | otherwise = [Multiple n y]
        helper n y (x:xs)
            | x == y    = helper (n+1) y xs
            | otherwise = helper n y [] ++ helper 1 x xs

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli [x] = [x,x]
dupli (x:xs) = dupli [x] ++ dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli list 0 = list
repli (x:xs) n = helper x n ++ repli xs n
    where
        helper x 1 = [x]
        helper x n = x : helper x (n-1)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list n = helper list n 1
    where
        helper [] _ _ = []
        helper (x:xs) n i
            | n == i = helper xs n 1
            | otherwise = x : helper xs n (i+1)

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x:xs) n 
    | n > 0     = (x:ys, zs)
    | otherwise = ([], x:xs)
    where
        (ys, zs) = split xs (n-1)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ _ 0 = []
slice (x:xs) 1 n = x:slice xs 1 (n-1)
slice (x:xs) l u = slice xs (l-1) (u-1)

slice2 :: [a] -> Int -> Int -> [a]
slice2 xs i j = [ x | (x,j) <- zip xs [1..j], i <= j]

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) i
    | i > 0     = rotate (xs ++ [x]) (i-1)
    | otherwise = rotate (x:xs) (length (x:xs) + i)

rotate2 :: [a] -> Int -> [a]
rotate2 xs n | n >= 0 = drop n xs ++ take n xs
             | n < 0  = drop m xs ++ take m xs
             where
                m = n + length xs

-- Problem 20
removeAt :: Int -> [a] -> ([a], [a])
removeAt 0 xs = ([], xs)
removeAt 1 (x:xs) = ([x], xs)
removeAt i xs
    | i > length xs = ([], xs)
    | otherwise     = (ys, head xs:zs)
    where
        (ys,zs) = removeAt (i-1) (tail xs)
