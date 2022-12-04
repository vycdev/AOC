module Day4
  ( main
  )
  where

import Prelude

import Data.Array (foldl)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day4/input.txt"

formPairs :: String -> Array (Array String)
formPairs s = split (Pattern ",") <$> split (Pattern "\r\n") s

parsePairs :: Array (Array String) -> Array (Array (Array (Maybe Int)))
parsePairs a = (\x -> (\y -> fromString <$> y) <$> (split (Pattern "-") <$> x)) <$> a

checkContainment :: Array (Array (Maybe Int)) -> Int -- [[(Just 10),(Just 83)],[(Just 2),(Just 10)]]
checkContainment [[Just a1, Just a2], [Just b1, Just b2]] 
    | a1 >= b1 && a2 <= b2 = 1
    | b1 >= a1 && b2 <= a2 = 1
    | otherwise = 0
checkContainment _ = 0

checkOverlapping :: Array (Array (Maybe Int)) -> Int -- [[(Just 10),(Just 83)],[(Just 2),(Just 10)]]
checkOverlapping [[Just a1, Just a2], [Just b1, Just b2]] 
    | a1 <= b2 && a2 >= b1 = 1
    | b1 <= a2 && b2 >= a1 = 1
    | otherwise = 0
checkOverlapping _ = 0

main :: Effect Unit
main = do 
    a <- input
    let pairs =  parsePairs $ formPairs a
    let solution1 = sum $ checkContainment <$> pairs
    let solution2 = sum $ checkOverlapping <$> pairs
    
    logShow solution1
    logShow solution2