import Graphics.Gloss

rewrite :: String -> String
rewrite [] = ""
rewrite ('F' : cs) = "F+F--F+F" ++ rewrite cs
rewrite (c : cs)   = c : rewrite cs

power f 0 x = x
power f n x = f (power f (n - 1) x)

delta = pi / 3

eval (x, y, a) [] = [(x, y)]
eval (x, y, a) ('+' : cs) = eval (x, y, a + delta) cs
eval (x, y, a) ('-' : cs) = eval (x, y, a - delta) cs
eval (x, y, a) ('F' : cs) = (x, y) : eval (x + cos a, y + sin a, a) cs

points = eval (0, 0, 0) "F+F--F+F--F+F--F+F--F+F--F+F"

snowflake = power rewrite 6 "F--F--F"

w :: Display
w = InWindow "Nice Window" (200, 200) (10, 10)

main :: IO ()
main = display w white (line (eval (0, 0, 0) snowflake))


