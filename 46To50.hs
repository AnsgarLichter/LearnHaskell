import Control.Monad (replicateM)
import GHC.CmmToAsm.AArch64.Instr (_d)
import Data.Ord (comparing)
import Data.List (sortBy, insertBy)
-- Problem 46
not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _       = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _         = True

xor' :: Bool -> Bool -> Bool
xor' True True   = False
xor' False False = False
xor' _ _         = True

nor' :: Bool -> Bool -> Bool
nor' a b = not' a `or'` b

nand' :: Bool -> Bool -> Bool
nand' a b = not' a `and'` b

--implies
impl' :: Bool -> Bool -> Bool
impl' a b = not' a `or'` b

-- equals
equ' :: Bool -> Bool -> Bool
equ' True  True  = True
equ' False False = True
equ' _     _     = False



table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStrLn $ concatMap (++ "\n") [ show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False] ]

-- Problem 47
-- infixl 4 `or'`
-- infixl 6 `and'`

-- Problem 48
infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'` -- was 7, changing it to 3 got me the same results as in the original question :(

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toString a ++ " " ++ show (f a) | a <- args n]
    where
        args n = replicateM n [True, False]
        toString = unwords . map (\x -> show x ++ " ")


-- Problem 49
gray :: Int -> [[Char]]
gray 0 = [""]
gray n = ['0' : x | x <- previous ] ++ [ '1' : x | x <- reverse previous ]
    where previous = gray (n-1)

-- Problem 50
data HTree a = Leaf a | Node (HTree a) (HTree a)
    deriving (Show)

huffman :: (Ord a, Ord w, Num w) => [(a,w)] -> [(a,[Char])]
huffman xs = sortBy (comparing fst) $ serialize $ htree $ sortBy (comparing fst) $ [(w, Leaf x) | (x,w) <- xs]
    where
        htree [(_,t)] = t
        htree ((w1,t1):(w2,t2):xs) = htree $ insertBy (comparing fst) (w1 + w2, Node t1 t2) xs
        serialize (Node l r) = 
            [(x, '0':code) | (x,code) <- serialize l] ++
            [(x, '1':code) | (x,code) <- serialize r]
        serialize (Leaf x) = [(x, "")]