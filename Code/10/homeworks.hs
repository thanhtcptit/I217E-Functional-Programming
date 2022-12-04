import Data.List

data Exp = Val Int | Add Exp Exp | Mul Exp Exp

data Instruction = Push | IVal Int | IAdd | IMul deriving Show
type Bytecode = [Instruction]

eval :: Exp -> Int
eval (Val n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

compile :: Exp -> Bytecode
compile (Val n) = [Push, IVal n]
compile (Add e1 e2) = (compile e1) ++ (compile e2) ++ [IAdd]
compile (Mul e1 e2) = (compile e1) ++ (compile e2) ++ [IMul]

pop2' 0 _ = []
pop2' n (x : xs) = x : pop2' (n - 1) xs

pop2 xs = pop2' (length xs - 2) xs

evalVal (IVal a) = a 

vm' :: Bytecode -> Int -> [Int] -> [Int]
vm' bc pc stack | pc >= length bc = stack
                | otherwise      = 
    case (bc !! pc) of
        Push -> vm' bc (pc + 2) (stack ++ [evalVal (bc !! (pc + 1))])
        IAdd -> vm' bc (pc + 1) ((pop2 stack) ++ [(stack !! (length stack - 1)) + (stack !! (length stack - 2))])
        IMul -> vm' bc (pc + 1) ((pop2 stack) ++ [(stack !! (length stack - 1)) * (stack !! (length stack - 2))])

vm bc = (vm' bc 0 []) !! (0)

-- >>> compile (Mul (Val 10) (Add (Val 20) (Val 30)))
-- [Push,IVal 10,Push,IVal 20,Push,IVal 30,IAdd,IMul]

-- >>> vm $ compile (Mul (Val 10) (Add (Val 20) (Val 30)))
-- 500
--

tc' :: Eq a => [(a, a)] -> [(a, a)]
tc' [] = []
tc' es = concat (map (\(e1, e2) -> [(e1, e3) | (e2', e3) <- es,
                                     e2 == e2' && elem (e1, e3) es == False]) es)

tc :: Ord a => [(a, a)] -> [(a, a)]
tc [] = []
tc es | tc' es == [] = es
      | otherwise    = sort $ nub $ tc $ es ++ (tc' es)

-- >>> tc [(0, 2), (1, 2), (2, 1), (2, 3), (3, 4), (3, 5), (4, 4)]
-- [(0,1),(0,2),(0,3),(0,4),(0,5),(1,1),(1,2),(1,3),(1,4),(1,5),(2,1),(2,2),(2,3),(2,4),(2,5),(3,4),(3,5),(4,4)]
--
