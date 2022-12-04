data Tree = Leaf | Node Tree Int Tree

post :: Tree -> [Int]
post Leaf             = []
post (Node Leaf x Leaf) = [x]
post (Node l x r)       = (post l) ++ (post r) ++ [x]

-- >>> post (Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 4 Leaf) 3 (Node Leaf 5 Leaf)))
-- [1,4,5,3,2]
--

numNode :: Tree -> Int
numNode Leaf = 0
numNode (Node l x r) = 1 + (numNode l) + (numNode r)

perfect :: Tree -> Bool
perfect Leaf = True
perfect (Node l _ r) = ((numNode l) == (numNode r)) && (perfect l) && (perfect r)

-- >>> perfect (Node (Node Leaf 1 Leaf) 2 (Node Leaf 4 Leaf))
-- True
--

f' :: [Int] -> Int -> [[Int]]
f' [] _ = [[]]
f' a@(x : xs) k | k > length a = [[]]
                | k == 0       = [[]]
                | otherwise    = if ((p !! 0) /= []) then concat [[v : t | t <- p, v < (t !! 0)] | v <- a]
                                 else [[v] | v <- a]
                    where p = f' xs (k - 1)

proxy :: [a] -> [[Int]] -> [[a]]
proxy xs ps = [[xs !! i | i <- p] | p <- ps]

f :: [a] -> Int -> [[a]]
f xs k = proxy xs (f' [0..(length xs) - 1] k)

-- >>> f [1, 2, 3, 4] 3
-- [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
--

prefix _ [] = [[]]
prefix [] (x : xs) = [x] : (prefix [x] xs)
prefix p (x : xs) = (p ++ [x]) : (prefix (p ++ [x]) xs)

suffix [] = [[]]
suffix a@(x : xs) = a : suffix xs

f1 [] = [[]]
f1 xs = concat [prefix [] p | p <- suffix xs, p /= []]

f2 xs = [t | t <- f1 xs, t /= []]

pairs :: [(Integer, Integer)]
pairs = [(x, y) | x <- [0..],  y <- [0..]]

-- >>> take 10 pairs
-- [[1],[1,2],[1,2,3],[2],[2,3],[3]]
--
