import Data.List (group)
-- Problem 1
myLast :: [a] -> a
myLast []     = error "An empty list does not have a last element"
myLast [x]    = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast []     = error "An empty list does not have a second-last element"
myButLast [x]    = error "A list with only one element does not have a second-last element"
myButLast (x:xs) =
    if length xs == 1
    then x
    else myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _    = error "Index out of bounds"
elementAt (x:_) 1 = x
elementAt (x:xs) i
    | i < 1     = error "Index out of bounds"
    | otherwise = elementAt xs (i - 1)

elementAt2 :: [a] -> Int -> a
elementAt2 xs n
    | length xs < n || n == 0 = error "Index out of bounds"
    | otherwise               = last $ take n xs

-- Problem 4
myLength :: [a] -> Int
myLength []     = 0
myLength [x]    = 1
myLength (_:xs) = let l = 1
                      help [] i = i
                      help (_:xs) i = help xs (i + 1)
                 in help xs l

myLength2 :: [a] -> Int
myLength2 []     = 0
myLength2 (_:xs) = 1 + myLength xs

myLength3 :: [a] -> Int
myLength3 = sum . map (const 1)


-- Problem 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x]

myReverse2 :: [a] -> [a]
myReverse2 = foldl (\a x -> x:a) []

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == myReverse list

isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 []  = True
isPalindrome2 [_] = True
isPalindrome2 xs  = head xs == last xs && isPalindrome2 (init (tail xs))

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a)      = [a]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == head xs = compress xs
    | otherwise = [x] ++ compress xs

compress2 :: Eq a => [a] -> [a]
compress2 = map head . group

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
    | x `elem` head (pack xs) = (x:head (pack xs)):tail (pack xs)
    | otherwise               = [x]:pack xs

pack2 :: Eq a => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x:first) : pack xs
    where
        getReps [] = ([], [])
        getReps (y:ys)
                | y == x    = let (f,r) = getReps ys
                              in (y:f, r)
                | otherwise = ([], y:ys)
        (first, rest) = getReps xs

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [ (length xs + 1, x) | (x:xs) <- group xs]

encode2 :: Eq a => [a] -> [(Int, a)]
encode2 [] = []
encode2 [x] = [(1, x)]
encode2 (x:xs) = help 1 x xs 
    where
        help n x []     = [(n, x)]
        help n x (y:ys)
            | x == y    = help (n+1) x ys
            | otherwise = (n,x) : help 1 y ys
