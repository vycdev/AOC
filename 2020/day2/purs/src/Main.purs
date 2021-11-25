module Main where

import Prelude
import Data.Array (filter, length, (!!))
import Data.Either (fromRight)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..))
import Data.String as S
import Data.String.CodeUnits (singleton, toChar, toCharArray)
import Data.String.Regex (Regex, regex, split)
import Data.String.Regex.Flags (global)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

processedRegex :: Regex
processedRegex = unsafePartial $ fromRight $ regex "[ ]|[:][ ]|[-]" global

parse :: Effect (Array (Array String))
parse = do
  input <- readTextFile UTF8 "src/input.txt"
  let
    firstSplit = S.split (Pattern "\r\n") input
  let
    secondSplit = map (split processedRegex) firstSplit
  pure secondSplit

type Password
  = { min :: Int, max :: Int, letter :: Char, pass :: String }

patternMatch :: Array String -> Password
patternMatch k = case k of
  [ min, max, letter, pass ] -> { min: unsafePartial fromJust (fromString min), max: unsafePartial fromJust (fromString max), letter: unsafePartial fromJust (toChar letter), pass: pass }
  _ -> { min: 1, max: 1, letter: '0', pass: "0" }

convertToPasswordArray :: Array (Array String) -> Array Password
convertToPasswordArray input = do
  let
    output = map (patternMatch) input
  output

numberOfInstances :: Char -> String -> Int
numberOfInstances letter pass = (length $ S.split (Pattern $ singleton letter) pass) - 1

solve1 :: Array Password -> Array Boolean
solve1 input = do
  map (\a -> a.min <= numberOfInstances a.letter a.pass && a.max >= numberOfInstances a.letter a.pass) input

solve2 :: Array Password -> Array Boolean
solve2 input = do
  map (\a -> ((toCharArray a.pass !! (a.min - 1)) == Just a.letter) /= ((toCharArray a.pass !! (a.max - 1)) == Just a.letter)) input

translateSolve :: Array Boolean -> Int
translateSolve a = length $ filter (_ == true) a

main :: Effect Unit
main = do
  parsedText <- parse
  logShow [ translateSolve $ solve1 $ convertToPasswordArray parsedText, translateSolve $ solve2 $ convertToPasswordArray parsedText ]
