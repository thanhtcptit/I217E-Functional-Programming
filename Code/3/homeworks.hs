import Data.List (sort)

data Tree = Leaf | Node Tree Int Tree deriving (Show, Eq)

member :: Int -> Tree -> Bool
member _ Leaf         = False
member x (Node l y r) = x == y || member x l || member x r

postorder :: Tree -> [Int]
postorder Leaf = []
postorder (Node l x r) = postorder l ++ postorder r ++ [x]

t0 = Node (Node Leaf 1 Leaf) 2
          (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf))

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup k ((x, y) : xs) | k == x = Just y
                         | otherwise  = myLookup k xs

group :: Ord a => [a] -> [a]
group [] = []
group [x] = [x]
group (x : y : ys) | x == y    = group (y : ys)
                   | otherwise = [x] ++ group (y : ys)

nub :: Ord a => [a] -> [a]
nub [] = []
nub xs = group (sort xs)
