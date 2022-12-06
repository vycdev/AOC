module Day6
  ( main
  )
  where

import Prelude

import Data.Array (findIndex, length, mapWithIndex, nub, slice)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day6/input.txt"

firstConsecutives :: Array Char -> Int -> Int -- My first solution is exceeding stack size apparently
firstConsecutives a k = if (length $ nub $ slice k (k+4) a) == 4 then k+3 else firstConsecutives a k+1  

group4 ∷ ∀ (a14 ∷ Type). Ord a14 ⇒ Array a14 → Array Int
group4 a = mapWithIndex (\i _ -> length $ nub $ slice i (i+4) a) a 
group14∷ ∀ (a2 ∷ Type). Ord a2 ⇒ Array a2 → Array Int
group14 a = mapWithIndex (\i _ -> length $ nub $ slice i (i+14) a) a 

main :: Effect Unit
main  = do 
    a <- input
    let charArray = toCharArray a


    -- logShow $ firstConsecutives charArray 0
    let solution1 = (fromMaybe 0 $ findIndex (\x -> x==4) $ group4 charArray) + 4
    let solution2 = (fromMaybe 0 $ findIndex (\x -> x==14) $ group14 charArray) + 14
    logShow $ solution1
    logShow $ solution2