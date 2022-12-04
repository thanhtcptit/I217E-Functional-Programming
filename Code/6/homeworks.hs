myUnzip [] = ([], [])
myUnzip ((x, y) : xys) = ([x] ++ a, [y] ++ b)
    where (a, b) = myUnzip xys

power [] = [[]]
power (x : xs) = [] : [x : ys | ys <- power xs] ++ [ys | ys <- power xs, ys /= []]


prefixes [] = [[]]
prefixes (x : xs) = [] : [ x : ys | ys <- prefixes xs ] 

suffixes [] = [[]]
suffixes all@(x : xs) = [all] ++ suffixes xs 

interleave y xs = [ a ++ [y] ++ b | (a, b) <- zip (prefixes xs) (suffixes xs)]

permutations [] = [[]]
permutations (x : xs) = [ zs | ys <- permutations xs, zs <- interleave x ys ]

ok [] = False
ok xs = (xs !! 0) + (xs !! 1) + (xs !! 2) == 15 &&
        (xs !! 3) + (xs !! 4) + (xs !! 5) == 15 &&
        (xs !! 6) + (xs !! 7) + (xs !! 8) == 15 &&
        (xs !! 0) + (xs !! 3) + (xs !! 6) == 15 &&
        (xs !! 1) + (xs !! 4) + (xs !! 7) == 15 &&
        (xs !! 2) + (xs !! 5) + (xs !! 8) == 15 &&
        (xs !! 0) + (xs !! 4) + (xs !! 8) == 15 &&
        (xs !! 2) + (xs !! 4) + (xs !! 6) == 15

magicSquare = [xs | xs <- permutations [1..9], ok xs]


data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node l y r) | x == y    = True
                      | x < y     = member x l
                      | otherwise = member x r

add :: Ord a => a -> Tree a -> Tree a
add x Leaf = Node Leaf x Leaf
add x (Node l y r) | x == y    = Node l y r
                   | x < y     = Node (add x l) y r
                   | otherwise = Node l y (add x r)

findLargest :: Ord a => Tree a -> Maybe a
findLargest Leaf = Nothing
findLargest (Node _ x Leaf) = Just x
findLargest (Node _ _ r) = findLargest r

delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node l y Leaf)    | x == y    = l
                            | x < y     = Node (delete x l) y Leaf
                            | otherwise = Leaf
delete x (Node l y r)       | x == y    = Node (delete z l) z r
                            | x < y     = Node (delete x l) y r
                            | otherwise = Node l y (delete x r)
    where Just z = findLargest l
