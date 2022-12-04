-- name: Tran Cong Thanh
-- id: 2210421
-- acknowledgements:

import System.Environment
import Graphics.Gloss

type State = [String]

margin :: Float
margin = 20

size :: Float
size = 15

box :: Float -> Float -> Color -> Picture
box x y c =
  Color c (Polygon [(x * margin,        y * margin),
                    (x * margin + size, y * margin),
                    (x * margin + size, y * margin + size),
                    (x * margin,        y * margin + size)])

draw :: State -> Picture
draw grid =
  Pictures [ box x y (
    case k of
        _ | k == '#' -> yellow
        _ | k == 'h' -> blue
        _ | k == 't' -> red
        _    -> black
  )
           | (i, row) <- zip [-5..] grid,
             (j, k)   <- zip [-10..] row,
             let x = fromIntegral j,
             let y = fromIntegral (-i)
           ]

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..] xs

countNeighbor :: Int -> Int -> [String] -> Int
countNeighbor _ _ [[]] = 0
countNeighbor r c grid = 
    (if (r > 0 && (grid !! (r - 1)) !! c == 'h') then 1 else 0) + (if (r < nRows - 1 && (grid !! (r + 1)) !! c == 'h') then 1 else 0) +
    (if (c > 0 && (grid !! r) !! (c - 1) == 'h') then 1 else 0) + (if (c < nCols - 1 && (grid !! r) !! (c + 1) == 'h') then 1 else 0) +
    (if (r > 0 && c > 0 && (grid !! (r - 1)) !! (c - 1) == 'h') then 1 else 0) + (if (r < nRows - 1 && c < nCols - 1 && (grid !! (r + 1)) !! (c + 1) == 'h') then 1 else 0) +
    (if (r > 0 && c < nCols - 1 && (grid !! (r - 1)) !! (c + 1) == 'h') then 1 else 0) + (if (r < nRows - 1 && c > 0 && (grid !! (r + 1)) !! (c - 1) == 'h') then 1 else 0)
    where
        nRows = length grid
        nCols = length (grid !! 0)

next :: a -> b -> State -> State
next _ _ grid = [ [ (case k of
    _ | k == 'h' -> 't'
    _ | k == 't' -> '#'
    _ | k == '#' -> if n > 0 && n <= 2 then 'h' else k where n = countNeighbor r c grid 
    _            -> '.'
    ) 
    | (c, k) <- enumerate row ] | (r, row) <- enumerate grid ]

window :: Display
window = InWindow "Wire World" (1000, 500) (100, 100)


main :: IO ()
main = do
  file : _ <- getArgs
  grid <- readFile file
  simulate window black 3 (lines grid) draw next
