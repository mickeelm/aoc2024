module Main where

import System.Environment (getArgs)
import Data.List
import Data.Universe.Helpers (diagonals)
main :: IO ()
main = do
    args <- getArgs
    input <- readFile "resources/input.txt"
    example <- readFile "resources/example.txt"
    case args of
        ["1"] -> print $ part1 input
        ["1x"] -> print $ part1 example
        ["2"] -> print $ part2 input
        ["2x"] -> print $ part2 example
        _ -> error "MERRY CHRISTMAS!"

part1 :: String -> Int
part1 contents = do
    let asLines = lines contents
    let horizontal = countXmas asLines
    let vertical = countXmas $ transpose asLines
    let southWestNorthEast = countXmas $ diagonals asLines
    let northWestSouthEast = countXmas $ diagonals $ reverse asLines
    horizontal + vertical + southWestNorthEast + northWestSouthEast

part2 :: String -> Int
part2 contents = sumInput $ lines contents

sumInput :: [String] -> Int
sumInput [a,b,c] = countMasX a (tail b) c 
sumInput (a:b:c:xs) = countMasX a (tail b) c + sumInput (b:c:xs)
sumInput _ = error "Corrupted input"

countMasX :: String -> String -> String -> Int
countMasX [x1,_,x3] [y,_] [z1,_,z3] = if isMasX x1 x3 y z1 z3 then 1 else 0 
countMasX (x1:x2:x3:xs) (y:ys) (z1:z2:z3:zs) = (if isMasX x1 x3 y z1 z3 then 1 else 0) + countMasX (x2:x3:xs) ys (z2:z3:zs)
countMasX _ _ _ = error "Corrupted Input"

isMasX :: Char -> Char -> Char -> Char -> Char -> Bool
isMasX upperLeft upperRight center lowerLeft lowerRight
    | center /= 'A' = False
    | mAndS upperLeft lowerRight && mAndS upperRight lowerLeft = True
    | otherwise  = False

mAndS :: Char -> Char -> Bool
mAndS 'S' 'M' = True
mAndS 'M' 'S' = True
mAndS _ _ = False

countXmas :: [String] -> Int
countXmas contents = do
    let forward = sum $ map (`countLineXmas` 0) contents
    let reversed_ = sum $ map ((`countLineXmas` 0) . reverse) contents
    forward + reversed_

countLineXmas :: String -> Int -> Int
countLineXmas [] count = count
countLineXmas "XMAS" count = count + 1
countLineXmas ('X':'M':'A':'S':xs) count = countLineXmas xs (count + 1)
countLineXmas xs count = countLineXmas (tail xs) count
