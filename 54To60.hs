import GHC.CmmToAsm.AArch64.Instr (_d)
import Data.Type.Coercion (sym)
data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Problem 55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch 'x' left right | i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]

-- Problem 56
mirror :: Tree a -> Tree a -> Bool
mirror Empty          Empty          = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _              _              = False

symmetric :: Tree a -> Bool
symmetric Empty          = True
symmetric (Branch _ l r) = mirror l r

-- Problem 57
add :: Ord a => a -> Tree a -> Tree a
add x Empty            = leaf x
add x t@(Branch y l r) = case compare x y of
                          LT -> Branch y (add x l) r
                          GT -> Branch y l (add x r)
                          EQ -> t

construct :: Ord a => [a] -> Tree a
construct xs = foldl (flip add) Empty xs

-- Problem 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

symCbalTrees2 :: Int -> [Tree Char]
symCbalTrees2 xs = [ x | x <- cbalTree 5, symmetric x]

-- Problem 59
hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [leaf x]
hbalTree x h = [ Branch x l r | (hl, hr) <- [(h-2, h-1), (h-1,h-1), (h-1, h-2)], l <- hbalTree x hl, r <- hbalTree x hr ]

-- Problem 60
hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes a n = [ x | y <- [minHeight n .. maxHeight n], x <- hbalTree a y, n == countNodes x ]
    where
        minHeight nodes = ceiling $ logBase 2 $ fromIntegral (nodes+1)
        maxHeight nodes = length (takeWhile (<= nodes+1) fibs) - 3
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

        countNodes Empty = 0
        countNodes (Branch _ l r) = 1 + countNodes l + countNodes r