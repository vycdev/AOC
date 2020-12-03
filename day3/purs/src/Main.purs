module Main where

import Prelude
import Data.Array (length, (!!))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

parse :: Effect (Array (Array String))
parse = do
  input <- readTextFile UTF8 "input.txt"
  let
    firstSplit = split (Pattern "\r\n") input
  let
    secondSplit = map (\a -> split (Pattern "") a) firstSplit
  pure secondSplit

type Vector2
  = { x :: Int
    , y :: Int
    }

solve :: Vector2 -> Vector2 -> Int -> Array (Array String) -> Int
solve velocity position sum input = do
  let
    newPos = { y: (position.y + velocity.y), x: fromMaybe 0 (map (mod (position.x + velocity.x)) ((length <$> input) !! position.y)) } :: Vector2
  if position.y <= length input then
    if (fromMaybe "." $ join $ map (_ !! position.x) $ input !! position.y) == "#" then
      solve velocity newPos (sum + 1) input
    else
      solve velocity newPos sum input
  else
    sum

main :: Effect Unit
main = do
  a <- parse
  let
    velocity = { y: 1, x: 3 } :: Vector2
  let
    startpos = { y: 0, x: 0 } :: Vector2
  logShow $ solve velocity startpos 0 a

-- to solve the part2 just multiply the functions with the corresponding velocities from the website
