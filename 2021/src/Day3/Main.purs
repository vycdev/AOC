module Day3 where

import Prelude

import Data.Array (concat, filter, foldl, index, length, zipWith)
import Data.Digit (_zero, fromChar, toInt)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (pow, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

textFile :: Effect String
textFile = readTextFile UTF8 "src/Day3/input.txt"

isIt1 :: Int -> Int -> Int 
isIt1 number length' 
    | toNumber (number) / toNumber (length') >= 0.5 = 1
    | otherwise = 0

isIt1reverse :: Int -> Int -> Int 
isIt1reverse number length' 
    | toNumber (number) / toNumber (length') >= 0.5 = 0
    | otherwise = 1


mostCommonBits :: Array (Array Int) -> Array Int
mostCommonBits data2 = map (\a -> isIt1 a (length data2)) addedArra  
    where 
    addedArra = foldl (\acc a -> zipWith (+) acc a) [0,0,0,0,0,0,0,0,0,0,0,0,0,0] data2
        
mostUncommonBits :: Array (Array Int) -> Array Int
mostUncommonBits data2 = map (\a -> isIt1reverse a (length data2)) addedArra  
    where 
    addedArra = foldl (\acc a -> zipWith (+) acc a) [0,0,0,0,0,0,0,0,0,0,0,0,0,0] data2

binaryArrayToInt :: Array Int -> Int
binaryArrayToInt arr = foldrWithIndex (\i v acc -> acc + v * (pow 2 (length arr - i - 1))) 0 arr

getOxygen :: Array (Array Int) -> Array Int   
getOxygen d = go d (mostCommonBits d) 0 
    where 
    go :: Array (Array Int) -> Array Int -> Int -> Array Int 
    go [da] _ _         = da    
    go da _ 20          = concat da
    go arr commonBits i = go (remainingNumbers) (mostCommonBits remainingNumbers) (i + 1)
        where 
        remainingNumbers = (filter (\bits -> (commonBitAtI commonBits) == (commonBitAtI bits)) arr)
            where 
            commonBitAtI bits = case index bits i of 
                (Just bit) -> bit
                _ -> -1  

getCO2 :: Array (Array Int) -> Array Int   
getCO2 d = go d (mostUncommonBits d) 0 
    where 
    go :: Array (Array Int) -> Array Int -> Int -> Array Int 
    go [da] _ _ = da    
    go da _ 20 = concat da 
    go arr commonBits i = go (remainingNumbers) (mostUncommonBits remainingNumbers) (i + 1)
        where 
        remainingNumbers = (filter (\bits -> (commonBitAtI commonBits) == (commonBitAtI bits)) arr)
            where 
            commonBitAtI bits = case index bits i of 
                (Just bit) -> bit
                _ -> -1  

main :: Effect Unit
main = do 
    -- parsing 
    content <- textFile
    let data1 = (\a -> toCharArray a) <$> (lines content)
    let data2 = map (\a -> toInt $ fromMaybe _zero (fromChar a)) <$> data1
    -- part1 
    let gamma' = binaryArrayToInt $ mostCommonBits data2 
    let epsilon = binaryArrayToInt $ mostUncommonBits data2
    logShow $ gamma' * epsilon
    -- part2
    let oxygen = getOxygen data2
    let co2 = getCO2 data2
    logShow oxygen
    logShow co2 
    logShow $ (binaryArrayToInt oxygen) * (binaryArrayToInt co2) 


-- PART 2 IS BROKEN AAAAAAAAAAAA