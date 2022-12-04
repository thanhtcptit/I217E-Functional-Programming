factorial :: Int -> Int
factorial x = if x == 0 then 1 else x * factorial (x - 1)

fib :: Int -> Int
fib x | x == 0    = 0
      | x == 1    = 1
      | otherwise = fib (x - 1) + fib (x - 2)

myGcd :: Int -> Int -> Int
myGcd x y = if y == 0 then x else if y > x then myGcd y x else myGcd (x - y) y


sumList :: [Int] -> Int
sumList []       = 0
sumList (x : xs) = x + sumList xs

evens :: [a] -> [a]
evens []  = []
evens [x] = [x]
evens (x : _ : xs) = x : evens xs

range :: Int -> Int -> [Int]
range m n | m > n     = []
          | otherwise = m : range (m + 1) n

insert :: Int -> [Int] -> [Int]
insert a []       = [a]
insert a (x : xs) = if a > x then x : insert a xs else a : x : xs

isort :: [Int] -> [Int]
isort []       = []
isort (x : xs) = insert x (isort xs)
