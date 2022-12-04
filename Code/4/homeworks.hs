import Graphics.Gloss

rewrite :: [(Char, String)] -> String -> String
rewrite _ [] = []
rewrite ps (x : xs) = s ++ rewrite ps xs
    where s = case lookup x ps of
            Just t -> t
            Nothing -> [x]

power f 0 x = x
power f n x = f (power f (n - 1) x)

eval :: (Float, Float, Float) -> [Char] -> [(Float, Float)]
eval (x, y, _) [] = [(x, y)]
eval (x, y, a) ('+' : cs) = eval (x, y, a + delta) cs
eval (x, y, a) ('-' : cs) = eval (x, y, a - delta) cs
eval (x, y, a) ('F' : cs) = (x, y) : eval (x + cos a, y + sin a, a) cs
eval (x, y, a) (_ : cs)   = (x, y) : eval (x + cos a, y + sin a, a) cs

delta = 2 * pi / 3
start = "F-G-G"
rules = [('F', "F-G+F+G-F"), ('G', "GG")]

w :: Display
w = InWindow "Window" (800, 600) (100, 100)

main :: IO ()
main = display w white (line (eval (0, 0, 0) (power (rewrite rules) 6 start)))
