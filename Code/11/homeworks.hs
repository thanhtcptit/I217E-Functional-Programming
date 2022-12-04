check :: Int -> Bool
check 1 = True
check v
  | v `mod` 2 /= 0 && v `mod` 3 /= 0 && v `mod` 5 /= 0 = False
  | v `mod` 2 == 0 = check $ v `div` 2
  | v `mod` 3 == 0 = check $ v `div` 3
  | v `mod` 5 == 0 = check $ v `div` 5

hm :: Int -> [Int]
hm n = take n (1 : [x | x <- [2..], check x])