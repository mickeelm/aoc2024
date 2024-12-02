{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import Data.List (sort, group)
import Data.Bifunctor (bimap)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.Environment (getArgs)
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
part1 contents = do
    let rows = map parseInput $ lines contents
    let sortedLocations = sortLocations . locations $ rows
    let answer = sum $ uncurry (zipWith distance) sortedLocations
    answer

part2 :: String -> Int
part2 contents = do
    let rows = map parseInput $ lines contents
    let locations_ = locations rows
    let rightGrouped = listToOccurrenceMap $ snd locations_
    let similarityScore_ = similarityScore (fst locations_) rightGrouped
    similarityScore_

listToOccurrenceMap :: [Int] -> Map.Map Int Int
listToOccurrenceMap = Map.fromList . map (\x -> (head x, length x)) . group . sort

similarityScore :: [Int] -> Map.Map Int Int -> Int
similarityScore left rigthGrouped = foldl (\acc x -> acc + x * Map.findWithDefault 0 x rigthGrouped) 0 left

distance :: Int -> Int -> Int
distance a b = abs (a - b)

sortLocations :: ([Int], [Int]) -> ([Int], [Int])
sortLocations = bimap sort sort

locations :: [(Int, Int)] -> ([Int],[Int])
locations = foldl (\acc x -> bimap (fst x:) (snd x:) acc) ([],[])

parseInput :: String -> (Int, Int)
parseInput str = case parse pRow "" (pack str) of
    Left _ -> error "Shit hit the fan"
    Right row -> row

pRow :: Parser (Int, Int)
pRow = do
    location1 <- L.decimal
    _ <- string "   "
    location2 <- L.decimal
    return (location1, location2)
