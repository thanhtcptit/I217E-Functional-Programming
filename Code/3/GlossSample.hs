import Graphics.Gloss

w :: Display
w = InWindow "Nice Window" (200, 200) (10, 10)

main :: IO ()
main = display w white (Circle 80)