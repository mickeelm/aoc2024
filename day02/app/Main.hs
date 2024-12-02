{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Environment (getArgs)

import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
type Parser = Parsec Void Text

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
part1 contents =  do
    let parsed = map parseInput $ lines contents
    let orderedAndPaired = map (zipWithNext . reverseIfDecreasing) parsed
    length $ filter (all isSafe) orderedAndPaired

part2 :: String -> Int
part2 contents =  do
    let parsed = map parseInput $ lines contents
    let ordered = map reverseIfDecreasing parsed
    let pairedCombos = map (zipWithNextInner . combos) ordered
    length $ filter dampenerSafe pairedCombos

dampenerSafe :: [[(Int, Int)]] -> Bool
dampenerSafe = any (all isSafe)

combos :: [Int] -> [[Int]]
combos xs = xs:subCombos xs

subCombos :: [Int] -> [[Int]]
subCombos xs = do
    let idxZipped = zip [1..length xs] xs
    map (`flattenIgnoring` idxZipped) [1..length xs]

flattenIgnoring :: Int -> [(Int, Int)] -> [Int]
flattenIgnoring idx xs = map snd (filter (\x -> fst x /= idx) xs)

isSafe :: (Int, Int) -> Bool
isSafe pair = uncurry (<) pair && validDiff pair

validDiff :: (Int,Int) -> Bool
validDiff pair = (\x -> (x >= 1) && (x <= 3)) (snd pair - fst pair)

reverseIfDecreasing :: [Int] -> [Int]
reverseIfDecreasing xs
    | head xs > last xs = reverse xs
    | otherwise = xs

zipWithNext :: [a] -> [(a,a)]
zipWithNext xs = zip xs (tail xs)

zipWithNextInner :: [[a]] -> [[(a,a)]]
zipWithNextInner = map zipWithNext

parseInput :: String -> [Int]
parseInput str = case parse rowParser "" (pack str) of
    Left _ -> error "Shit hit the fan"
    Right row -> row

rowParser :: Parser [Int]
rowParser = L.decimal `sepBy` char ' '