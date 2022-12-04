data AVLTree a = Leaf | Node (AVLTree a) Int Int (AVLTree a) deriving Show

depth Leaf           = 0
depth (Node l _ d r) = d

createNode l x r = Node l x d r
    where d = 1 + max (depth l) (depth r)

slope Leaf = 0
slope (Node l _ _ r) = depth l - depth r

rotl (Node l x _ (Node m y _ r)) = createNode (createNode l x m) y r

rotr (Node (Node l x _ m) y _ r) = createNode l x (createNode m y r)

shiftl (Node l x d r) | slope r == 1 = rotl $ Node l x d (rotr r)
                      | otherwise    = rotl (Node l x d r)

shiftr (Node l x d r) | slope l == -1 = rotr $ Node (rotl l) x d r
                      | otherwise     = rotr (Node l x d r)

rebalance :: AVLTree a -> AVLTree a
rebalance t | slope t == 2  = shiftr t
            | slope t == -2 = shiftl t
            | otherwise     = t

add x Leaf = Node Leaf x 1 Leaf
add x (Node l y d r) | x == y    = Node l y d r
                     | x < y     = rebalance $ createNode (add x l) y r
                     | otherwise = rebalance $ createNode l y (add x r)

findLargest Leaf = Nothing
findLargest (Node _ x _ Leaf) = Just x
findLargest (Node _ _ _ r)    = findLargest r

delete _ Leaf = Leaf
delete x (Node l y _ Leaf)    | x == y    = l
                              | x < y     = rebalance $ createNode (delete x l) y Leaf
                              | otherwise = Leaf
delete x (Node l y _ r)       | x == y      = rebalance $ createNode (delete z l) z r
                              | x < y       = rebalance $ createNode (delete x l) y r
                              | otherwise   = rebalance $ createNode l y (delete x r)
    where Just z = findLargest l


primes' (x : xs) = x : [y | y <- primes' xs, y `mod` x /= 0]
primes = primes' [2..]