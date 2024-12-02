{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Environment (getArgs)

import Data.Text (Text, pack, unpack)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Replace.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Either (rights)
type Parser = Parsec Void Text

main :: IO ()
main = do
    args <- getArgs
    input <- readFile "resources/input.txt"
    example <- readFile "resources/example.txt"
    example2 <- readFile "resources/example2.txt"
    case args of
        ["1"] -> print $ part1 input
        ["1x"] -> print $ part1 example
        ["2"] -> print $ part2 input
        ["2x"] -> print $ part2 example2
        _ -> error "MERRY CHRISTMAS!"

part1 :: String -> Int
part1 = mulSum

part2 :: String -> Int
part2 contents = mulSum $ parseEnableDisableLine contents

mulSum :: String -> Int
mulSum instr = sum $ parseLine instr

parseLine :: String -> [Int]
parseLine str = case parse rowParser "" (pack str) of
    Left _ -> error "Shit hit the fan"
    Right row -> rights row

parseEnableDisableLine :: String -> String
parseEnableDisableLine str = case parse doDontSplitParser "" (pack str) of
    Left _ -> error "Shit hit the fan"
    Right row -> assembleEnabled $ asStrings row

rowParser :: Parser [Either Text Int]
rowParser = sepCap mulParser

mulParser :: Parser Int
mulParser = do
    _ <- string "mul("
    multiplier <- L.decimal
    _ <- char ','
    multiplicand <- L.decimal
    _ <- char ')'
    return (multiplier * multiplicand)

doParser :: Parser Text
doParser = string "do()"

dontParser :: Parser Text
dontParser = string "don't()"

doDontSplitParser :: Parser [Either Text Text]
doDontSplitParser = sepCap (doParser <|> dontParser)

asStrings :: [Either Text Text] -> [String]
asStrings = map (either unpack unpack)

assembleEnabled :: [String] -> String
assembleEnabled [] = error "Invalid input"
assembleEnabled [x] = x
assembleEnabled (x:xs)
    | current == "do()" = assembleEnabled $ x:tail xs
    | current == "don't()" = assembleEnabled $ x:tail (tail xs)
    | otherwise = assembleEnabled $ (x ++ current):tail xs
    where current = head xs
