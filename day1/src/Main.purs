module Main where

import Prelude

import Data.Array (mapMaybe, (!!))
import Data.Int (fromString)

import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (logShow)

import Node.FS.Sync (readTextFile)

part1 :: Array Int -> Array Int
part1 arr = do 
  a <- arr 
  b <- arr
  if a+b == 2020 then [a*b] else []

part2 :: Array Int -> Array Int
part2 arr = do 
  a <- arr 
  b <- arr
  c <- arr
  if a+b+c == 2020 then [a*b*c] else []

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "src/input.txt"
  let arrayOfStrings = split (Pattern "\r\n") content
  let arrayOfNumbers = mapMaybe (fromString) arrayOfStrings

  logShow (part2 arrayOfNumbers)