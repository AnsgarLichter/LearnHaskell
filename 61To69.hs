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

treeNodes :: Tree a -> Int
treeNodes Empty = 0
treeNodes (Branch _ l r) = 1 + treeNodes l + treeNodes r

treeEqual :: Tree a1 -> Tree a2 -> Bool
treeEqual Empty Empty                       = True
treeEqual (Branch _ l1 r1) (Branch _ l2 r2) = treeEqual l1 l2 && treeEqual r1 r2
treeEqual _                _                = False

isCompleteBinaryTree :: Tree Char -> Bool
isCompleteBinaryTree t = treeEqual t $ completeBinaryTree2 $ treeNodes t

-- Problem 64
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

layout :: Tree a -> Tree (a,(Int, Int))
layout t = fst (helper 1 1 t)
    where
        helper x y Empty          = (Empty, x)
        helper x y (Branch a l r) = (Branch (a, (x',y)) l' r', x'')
            where
                (l', x')  = helper x (y+1) l
                (r', x'') = helper (x'+1) (y+1) r
        

-- Problem 65
tree65 :: Tree Char
tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

depth :: Tree a -> Int
depth Empty          = 0
depth (Branch _ l r) = max (depth l) (depth r) + 1

leftDepth :: Tree a -> Int
leftDepth Empty          = 0
leftDepth (Branch _ l r) = leftDepth l + 1

layout2 :: Tree a -> Tree(a, (Int, Int))
layout2 t = helper x1 1 sep1 t
    where
        d = depth t
        ld = leftDepth t
        x1 = 2^(d-1) - 2^(d-ld) + 1
        sep1 = 2^(d-2)
        helper _ _ _ Empty            = Empty
        helper x y sep (Branch a l r) = Branch (a, (x,y))
                                        (helper (x-sep) (y+1) (sep `div` 2) l)
                                        (helper (x+sep) (y+1) (sep `div` 2) r)

-- Problem 66 (works for the tree shown in the problem, may not work for other trees)
rightDepth :: Tree a -> Int
rightDepth Empty          = 0
rightDepth (Branch _ l r) = 1 + rightDepth r

layout3 :: Tree a -> Tree (a, (Int, Int))
layout3 t = helper x1 1 t
    where
        x1 = leftDepth t + 1
        helper _ _ Empty = Empty
        helper x y (Branch a l r) = Branch (a, (x,y))
                                    (helper xl (y+1) l)
                                    (helper xr (y+1) r)
            where
                xl = if y == 1 then x-rightDepth l else x-1
                xr = if y == 1 then x+leftDepth r  else x+1

-- Problem 67A
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = x : '(' : treeToString l ++ "," ++ treeToString r

stringToTree :: (MonadFail m) => String -> m (Tree Char)
stringToTree ""  = return Empty
stringToTree [x] = return $ Branch x Empty Empty
stringToTree str = parse str >>= \("",t) -> return t
    where
        parse a@(x:_)
            | x == ',' || x == ')' = return (a, Empty)
        parse (x:y:xs)
            | y == ',' || y == ')' = return (y:xs, Branch x Empty Empty)
            | y == '('             = do (',':xs', l) <- parse xs
                                        (')':xs'', r) <- parse xs'
                                        return (xs'', Branch x l r)
        parse _                    = fail "bad string"

-- Problem 68 (partially)
treeToPreorder :: Tree Char -> String
treeToPreorder = preorder
    where
        preorder Empty = ""
        preorder (Branch a l r) = a : preorder l ++ preorder r

treeToInorder :: Tree Char -> String
treeToInorder = inorder
    where
        inorder Empty = ""
        inorder (Branch a l r) = inorder l ++ a : inorder r

-- Problem 69
example = "abd..e..c.fg..."
ds2tree :: String -> (Tree Char, [Char])
ds2tree [] = (Empty, "")
ds2tree ('.':xs) = (Empty,xs)
ds2tree (x:xs) = (Branch x left right, rest2)
    where (left,rest)   = ds2tree xs
          (right,rest2) = ds2tree rest

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x l r) = x:(tree2ds l ++ tree2ds r)