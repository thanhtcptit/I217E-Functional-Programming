sumList [] = 0
sumList (x : xs) = x + sumList xs

leibniz :: Int -> Float
leibniz n = sumList (take n [4 * i / (fromIntegral j :: Float) | j <- [1,3..],
                     let i = if (j - 1) `mod` 4 == 0 then 1 else -1])

data Exp = Val Int | Var String | Add Exp Exp | Mul Exp Exp
type Env = [(String , Int)]

find :: String -> [(String, Int)] -> Maybe Int
find n [] = Nothing
find n ((k, v) : ps) | n == k = Just v
                     | otherwise = find n ps

eval1 :: Env -> Exp -> Int
eval1 _ (Val x) = x
eval1 ps (Var n) | Just v <- lookup n ps = v
                 | otherwise             = error n
eval1 ps (Mul x y) = (eval1 ps x) * (eval1 ps y)
eval1 ps (Add x y) = (eval1 ps x) + (eval1 ps y)


eval2 :: Env -> Exp -> Either String Int
eval2 _ (Val x) = Right x
eval2 ps (Var n) = case find n ps of
    Just v -> Right v
    Nothing -> Left $ n ++ " is undefined"
eval2 ps (Mul x y) | Left a <- (eval2 ps x) = Left a
                   | Left b <- (eval2 ps y) = Left b
                   | Right a <- (eval2 ps x), Right b <- (eval2 ps y) = Right (a + b)

eval2 ps (Add x y) | Left a <- (eval2 ps x) = Left a
                   | Left b <- (eval2 ps y) = Left b
                   | Right a <- (eval2 ps x), Right b <- (eval2 ps y) = Right (a * b)

env' = [("x", 10 :: Int), ("y", 30)]
exp' = Mul (Add (Mul (Var "x") (Val 3)) (Var "y")) (Var "x")

-- >>> eval2 env' exp'
-- Right 400
--

