squareSum :: Int -> Int -> Int
squareSum x y = x * x + y * y

-- >>> squareSum 1 10
-- 101
--

sum1 :: Int -> Int
sum1 n = if n == 0 then 0 else n + sum1 (n - 1)

sum2 :: Int -> Int
sum2 n | n == 0    = 0
       | otherwise = n + sum2 (n - 1)

sum3 :: Int -> Int
sum3 0 = 0
sum3 n = n + sum3 (n - 1)

myLength :: [a] -> Int
myLength []       = 0
myLength (_ : xs) = 1 + myLength xs

append :: [a] -> [a] -> [a]
append []       ys = ys
append (x : xs) ys = x : append xs ys

sumList :: [Int] -> Int
sumList []       = 0
sumList (x : xs) = x + sumList xs

evens :: [a] -> [a]
evens []           = []
evens [x]          = [x]
evens (x : _ : zs) = x : evens zs