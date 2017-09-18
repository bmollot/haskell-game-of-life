import Control.Concurrent
import System.Process

initialGrid = [[0,0,1,0,0,0,0,0,0,0],
               [1,0,1,0,0,0,0,0,0,0],
               [0,1,1,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0]]

coordGrid grid = [[(x,y) | x <- [0..pred (length (head grid))]] | y <- [0..pred (length grid)]]

prettyPrint [] = putStrLn "|"
prettyPrint (x:xs) = do
    putStr (if x == 0 then "  " else "██")
    prettyPrint xs

display [] = print ""
display (x:[]) = prettyPrint x
display (x:xs) = do
    prettyPrint x
    display xs

cellVal grid x y
    | y < 0 || y >= length grid = 0
    | x < 0 || x >= length (head grid) = 0
    | otherwise = grid!!y!!x

neighbours grid x y =
    let getVal = cellVal grid in
    sum [getVal (pred x) (pred y), getVal x (pred y), getVal (succ x) (pred y),
         getVal (pred x) y,                           getVal (succ x) y,
         getVal (pred x) (succ y), getVal x (succ y), getVal (succ x) (succ y)]

nextVal grid x y
    | n == 2 && l = 1
    | n == 3 = 1
    | otherwise = 0
    where n = neighbours grid x y
          l = cellVal grid x y == 1

gridTick grid = map (map (\cell -> nextVal grid (fst cell) (snd cell))) (coordGrid grid)

main = start initialGrid

start lastGrid = let grid = gridTick lastGrid in do
    system "clear"
    display grid
    threadDelay 100000
    start grid