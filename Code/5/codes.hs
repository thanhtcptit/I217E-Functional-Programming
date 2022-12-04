myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct (x : []) = x
myProduct (x : y : xs) = x * myProduct (y : xs)

--- xs ++ [] = xs
--- We show the claim by structural induction on xs
--- Base case
--- If xs = [] then
---     lhs = [] ++ [] = []
---     rhs = []
--- Induction step
--- If xs = x : xs' for some x and xs' then
--- lhs = (x : xs') ++ [] = x : (xs' ++ []) = x : xs'
--- rhs = x : xs'
