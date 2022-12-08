module Day8
  ( main
  )
  where

import Prelude

import Data.Array (foldl, length, mapWithIndex, reverse, take, takeEnd, takeWhile, (!!))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)


input :: Effect String
input = readTextFile UTF8 "src/Day8/input.txt"

parseMatrix :: String -> Array (Array Int) 
parseMatrix s = (\x -> (\a -> fromMaybe 0 $ fromString a) <$> split (Pattern "") x) <$> split (Pattern "\r\n") s

getColumn :: Int -> Array (Array Int) -> Array Int
getColumn i a = (\row -> fromMaybe 0 $ row !! i) <$> a

isVisible :: Int -> Array Int -> Boolean 
isVisible elm a = not $ foldl (\acc x -> acc || x) false $ (\x -> x >= elm) <$> a

isVisible' :: Array Int -> Int -> Boolean
isVisible' a i 
    | i == 0 || (i == (length a) - 1) = true
    | otherwise = (isVisible elm $ (take i) a) || (isVisible elm $ takeEnd ((length a) - i - 1) a)
    where elm = fromMaybe 0 $ a !! i 

getVisibleTreesMatrix :: Array (Array Int) -> Array (Array Boolean)
getVisibleTreesMatrix a = mapWithIndex (\i x -> mapWithIndex (\j _ -> (isVisible' x j) || (isVisible' (getColumn j a) i)) x) a 

getScore :: Array Int -> Int -> Int 
getScore a i = 
    let elm = fromMaybe 0 $ a !! i
        left = reverse $ take i a 
        right = takeEnd ((length a) - i - 1) a 
        leftVisible = (length $ takeWhile (_ < elm) left)
        rightVisible = (length $ takeWhile (_ < elm) right) 
    in (leftVisible + (if length left == leftVisible then 0 else 1)) * (rightVisible + (if length right == rightVisible then 0 else 1))

getScoresTreesMatrix :: Array (Array Int) -> Array (Array Int)
getScoresTreesMatrix a = mapWithIndex (\i x -> mapWithIndex (\j _ -> (getScore x j) * (getScore (getColumn j a) i)) x) a 

getMaximum :: Array Int -> Int
getMaximum a = foldl (\acc x -> if x > acc then x else acc) 0 a

main :: Effect Unit
main = do 
    inpt <- input
    let matrix = parseMatrix inpt
    let solution1 = sum $ foldl (\acc x -> if x then acc + 1 else acc) 0 <$> getVisibleTreesMatrix matrix 
    let solution2 = getMaximum $ getMaximum <$> getScoresTreesMatrix matrix

    logShow solution1
    logShow $ solution2