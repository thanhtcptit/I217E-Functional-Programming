data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

depth Leaf         = 0
depth (Node l _ r) = 1 + max (depth l) (depth r)

slope Leaf = 0
slope (Node l _ r) = abs (depth l - depth r)

