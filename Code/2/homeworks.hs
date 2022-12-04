sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs

oddplus1 :: [Int] -> [Int]
oddplus1 xs = [x + 1 | x <- xs, x `mod` 2 == 1]

insert :: Int -> [Int] -> [Int]
insert a []       = [a]
insert a (x : xs) = if a > x then x : insert a xs else a : x : xs

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge (x : xs) ys = merge xs (insert x ys)

split :: [Int] -> ([Int], [Int])
split [] = ([], [])
split [x] = ([x], [])
split (x : y : xs) = (x : ys, y: zs)
    where (ys, zs) = split xs

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where (ys, zs) = split xs

