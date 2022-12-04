test1 = do
  print ((\x -> x + 1) 2)     -- 3
  print ((\x y -> x * y) 2 3) -- 6
  print ((\x y -> x * y) 2 3) -- 6

-- >>> test1
-- 3
-- 6
-- 6
--

myMap :: (a -> b) -> [a] -> [b]
myMap f []       = []
myMap f (x : xs) = f x : myMap f xs

test2 = print (myMap (* 10) [1,-2,3,-4])

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x : xs) | p x       = x : myFilter p xs
                    | otherwise = myFilter p xs

test3 = print (myFilter (> 0) [1,-2,3,-4])

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p [] = ([], [])
partition p (x : xs)
  | p x       = (x : ys, zs) 
  | otherwise = (ys, x : zs) 
  where (ys, zs) = partition p xs

test4 = print (partition (> 0) [1,-2,3,-4])

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f e []       = e
myFoldl f e (x : xs) = myFoldl f (f e x) xs

test5 = print (myFoldl (-) 10 [1,2,3])

test6 = do
  print [ x * 10 | x <- [1,-2,3,-4] ]
  print [ x | x <- [1,-2,3,-4], x > 0 ]
  print [ x + y | x <- [10,20], y <- [1,2] ]
