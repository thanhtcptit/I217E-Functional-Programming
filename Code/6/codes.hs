myZip [] [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZipWith _ [] [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

prefixes [] = [[]]
prefixes (x : xs) = [] : [ x : ys | ys <- prefixes xs ] 

suffixes [] = [[]]
suffixes all@(x : xs) = [all] ++ suffixes xs 

interleave y xs = [ a ++ [y] ++ b | (a, b) <- zip (prefixes xs) (suffixes xs)]

permutations [] = [[]]
permutations (x : xs) = [ zs | ys <- permutations xs, zs <- interleave x ys ]

safe i j i' j' | i == i' && j == j' = True
               | i /= i' && j /= j' && abs (i - i') /= abs (j - j') = True
               | otherwise = False

ok xs = and [ safe i j i' j' | (i, j) <- ijs, (i', j') <- ijs]
    where ijs = zip [0..length xs - 1] xs

nqueen n = [xs | xs <- permutations [0..n - 1], ok xs]

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

member :: Eq a => a -> Tree a -> Bool
member _ Leaf         = False
member x (Node l y r) = x == y || member x l || member x r

depth :: Tree a -> Int
depth Leaf         = 0
depth (Node l _ r) = 1 + max (depth l) (depth r) 

inorder :: Tree a -> [a]
inorder Leaf         = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r


