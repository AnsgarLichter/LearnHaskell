data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Problem 61
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ l r) = let countL = if isLeaf l then 1 else 0
                                 countR = if isLeaf r then 1 else 0
                                 isLeaf (Branch _ Empty Empty) = True
                                 isLeaf _                      = False
                             in  countL + countR + countLeaves l + countLeaves r

-- Problem 61A
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch a l r)
    | isLeaf (Branch a l r) = a : leaves l ++ leaves r
    | otherwise = leaves l ++ leaves r
    where isLeaf (Branch _ Empty Empty) = True
          isLeaf _                      = False

-- Problem 62
internals :: Tree a -> [a]
internals Empty = []
internals (Branch a l r)
    | isLeaf (Branch a l r) = []
    | otherwise             = a : internals l ++ leaves r
    where isLeaf (Branch _ Empty Empty) = True
          isLeaf _                      = False

-- Problem 62B
atLevel :: Tree a -> Int -> [a]
atLevel Empty _          = []
atLevel (Branch a _ _) 1 = [a]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)

-- Problem 63
completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree 1 = Empty
completeBinaryTree h
    | h `mod` 2 == 0 = Branch 'x' (completeBinaryTree (h-1)) (completeBinaryTree (h-2))
    | otherwise      = Branch 'x' (completeBinaryTree (h-1)) Empty

completeBinaryTree2 :: Int -> Tree Char
completeBinaryTree2 n = generateTree 1
    where generateTree x
            | x > n     = Empty
            | otherwise = Branch 'x' (generateTree (2*x)) (generateTree (2*x+1))

isCompleteBinaryTree :: Tree Char -> Bool
isCompleteBinaryTree t = treeEqual t $ completeBinaryTree2 $ treeNodes t
    where
        treeNodes Empty = 0
        treeNodes (Branch _ l r) = 1 + treeNodes l + treeNodes r

        treeEqual Empty Empty                       = True
        treeEqual (Branch _ l1 r1) (Branch _ l2 r2) = treeEqual l1 l2 && treeEqual l2 r2
        treeEqual _                _                = False
