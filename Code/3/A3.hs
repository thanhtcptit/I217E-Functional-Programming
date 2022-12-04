data Tree = Leaf | Node Tree Int Tree deriving (Show, Eq)

member :: Int -> Tree -> Bool
member x Leaf         = False
member x (Node l y r) = x == y || member x l || member x r

t0 = Node (Node Leaf 1 Leaf) 2
          (Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf))

test1 = do
  print (member 1 t0)   -- True
  print (member 6 t0)   -- False

inorder :: Tree -> [Int]
inorder Leaf         = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

test2 = print (inorder t0) -- [1,2,3,4,5]

preorder :: Tree -> [Int]
preorder Leaf         = []
preorder (Node l x r) = x : preorder l ++ preorder r

test3 = print (preorder t0) -- [2,1,4,3,5]

mirror Leaf         = Leaf
mirror (Node l x r) = Node (mirror r) x (mirror l)

class Size a where
  size :: a -> Int

instance Size [a] where
  size xs = length xs

instance Size Tree where
  size Leaf         = 0
  size (Node l x r) = 1 + size l + size r

totalSize :: Size a => [a] -> Int
totalSize xs = sum [ size x | x <- xs ]


monthNames = [(1, "Jan"), (2, "Feb")]

monthName1 n =
  case lookup n monthNames of
    Just s  -> s
    Nothing -> "not found"

monthName2 n
  | Just s <- lookup n monthNames = s
  | otherwise = "not found"

-- >>> monthName2 1
-- "Jan"
--


partition1 p [] = ([], [])
partition1 p (x : xs)
  | p x       = (x : ys, zs)
  | otherwise = (ys, x : zs)
  where (ys, zs) = partition1 p xs

partition2 p [] = ([], [])
partition2 p (x : xs)
  | True == p x,  (ys, zs) <- partition2 p xs =
      (x : ys, zs)
  | (ys, zs) <- partition2 p xs =
      (ys, x : zs)

-- [ x + y | (x, y) <- [(1,10), (2,20)] ] -- [11,22] 
-- [ x | Just x <- [Just 1, Nothing, Just 2] ] -- [1,2]

main :: IO ()
-- main = putStr "Hello, World!\n"
-- main = do { putStr "Hello "; putStr "World!\n" }
main = do
  putStr "Hello "
  putStr "World!\n"

