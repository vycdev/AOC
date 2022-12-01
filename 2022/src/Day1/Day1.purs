module Day1
  ( main
  )
  where

import Prelude

import Data.Array (drop, length, sort, takeEnd, takeWhile, (:))
import Data.Foldable (maximum, sum)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day1/input.txt"
 
parse :: String -> (Array (Int))
parse s = (\a -> fromMaybe 0 a) <$> ((\a -> fromString a) <$> (split (Pattern "\r\n") s))

format :: Array Int -> Array (Array Int)
format arr = sadness arr []
  where
    sadness :: Array Int -> Array (Array Int) -> Array (Array Int)
    sadness [] new = new
    sadness old new = sadness (drop (length taken + 1) old) (taken : new)
      where taken = (takeWhile (_ /= 0) old) 

main ∷ Effect Unit
main = do
  content <- input
  let finalArray = sum <$> (format $ parse content)
  -- solution 1
  logShow $ maximum finalArray
  -- solution 2
  logShow $ sum $ takeEnd 3 (sort finalArray) 
