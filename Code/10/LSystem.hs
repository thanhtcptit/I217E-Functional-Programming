import Graphics.Gloss

type State = (Float,Float,Float)
type Stack = [State]

eval :: Stack -> State -> String -> Path
eval stack (x, y, theta)   []         = [(x, y)]
eval stack (x, y, theta)   ('+' : cs) = eval stack (x, y, theta + delta) cs
eval stack (x, y, theta)   ('-' : cs) = eval stack (x, y, theta - delta) cs
{-
eval ...   ...          ('[' : cs) = ...
eval ...   ...          (']' : cs) = ...
-}
eval stack (x, y, theta)   (c : cs)   = (x, y) : eval stack (x', y', theta) cs
  where x' = x + cos theta
        y' = y + sin theta

rewrite :: [(Char, String)] -> String -> String
rewrite rules []         = []
rewrite rules (c : cs)
  | Just s <- lookup c rules = s ++ rewrite rules cs
  | otherwise                = c :  rewrite rules cs

power f 0 x = x
power f n x = power f (n - 1) (f x)

-- Koch's snowflake
rules = [('F', "F+F--F+F")]
start = "F--F--F"
delta = pi / 3

-- Sierpinski's triangle
-- rules = [('F',"F-G+F+G-F"), ('G', "GG")]
-- start = "F-G-G"
-- delta = 2 * pi / 3

-- Sierpinski's triangle, another version
-- rules = [('A',"B-A-B"), ('B',"A+B+A")]
-- start = "A"
-- delta = pi / 3

-- Fractal plant
-- rules =
--    [('S', "++++++++++++++++++X"),
--     ('F', "FF"),
--     ('X', "F-[[X]+X]+F[+FX]-X")]
-- start = "S"
-- delta = 25 * pi / 180

picture = Line (eval [] (0,0,0) (power (rewrite rules) 9 start))

window = InWindow "L-System" (500, 500) (20,  20)

main :: IO ()
main = display window white picture