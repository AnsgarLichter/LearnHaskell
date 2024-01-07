import Control.Monad (replicateM)
import Data.List (groupBy, sortBy, tails)
import Data.Ord (comparing)
import GHC (TyClGroup (group_instds))
import System.Random

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x list 1 = x : list
insertAt x (y : ys) i = y : insertAt x ys (i - 1)

insertAt2 :: a -> [a] -> Int -> [a]
insertAt2 x xs n = take (n - 1) xs ++ [x] ++ drop (n - 1) xs

-- Problem 22
range :: Int -> Int -> [Int]
range i j
  | i <= j = i : range (i + 1) j
  | otherwise = []

-- Problem 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect xs n
  | n < 0 || n > length xs = error "Index out of bounds"
  | otherwise = do
      pos <-
        replicateM n $
          getStdRandom $
            randomR (0, length xs - 1)
      return [xs !! i | i <- pos]

rndSelect2 :: [a] -> Int -> IO [a]
rndSelect2 xs n = do
  gen <- getStdGen
  return $ [xs !! pos | pos <- randomRs (0, n) gen]

-- Problem 24
diffSelect :: Int -> Int -> IO [Int]
diffSelect i n = rndSelect [1 .. n] i

diffSelect2 :: Int -> Int -> IO [Int]
diffSelect2 i n = helper [1 .. n] i
  where
    helper _ 0 = return []
    helper [] _ = return []
    helper xs n = do
      pos <- getStdRandom $ randomR (0, length xs - 1)
      let remaining = take pos xs ++ drop (pos + 1) xs
      rest <- helper remaining (n - 1)
      return ((xs !! pos) : rest)

-- Problem 25
rndPermu :: [a] -> IO [a]
rndPermu [] = return []
rndPermu xs = rndSelect xs (length xs)

rndPermu2 :: [a] -> IO [a]
rndPermu2 [] = return []
rndPermu2 (x : xs) = do
  pos <- getStdRandom $ randomR (0, length xs - 1)
  rest <- rndPermu2 xs
  return $ let (ys, zs) = splitAt pos rest in ys ++ (x : zs)

-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y : ys | y : zs <- tails xs, ys <- combinations (n - 1) zs]

-- Problem 27 - copied from solution as I do not have any clue
combination :: Int -> [a] -> [[a]]
combination 0 _ = [[]]
combination _ [] = []
combination n (x : xs) = map (x :) (combination (n - 1) xs) ++ combination n xs

group3 :: (Eq a) => [a] -> [([a], [a], [a])]
group3 people =
  [ (group1, group2, group3)
    | group1 <- combination 2 people,
      let peopleNotInGroup1 = [x | x <- people, x `notElem` group1],
      group2 <- combination 3 peopleNotInGroup1,
      let group3 = [x | x <- people, x `notElem` group2]
  ]

group :: [Int] -> (Eq a) => [a] -> [[[a]]]
group _ [] = [[]]
group (n : ns) people =
  [ g : gs
    | g <- combination n people,
      let remaining = [x | x <- people, x `notElem` g],
      gs <- group ns remaining
  ]

-- Problem 28
lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)

lsort2 :: [[a]] -> [[a]]
lsort2 = sortBy (\xs ys -> compare (length xs) (length ys))

lsort3 :: [[a]] -> [[a]]
lsort3 =
  sortBy
    ( \xs ys ->
        let a = length xs
            b = length ys
         in if a > b
              then GT
              else
                if a < b
                  then LT
                  else GT
    )

lfsort :: [[a]] -> [[a]]
lfsort lists = concat $ lsort groups
  where
    groups = groupBy (\xs ys -> length xs == length ys) (lsort lists)