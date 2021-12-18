module Day5 where

import Prelude

import Data.Array (foldl, head, modifyAtIndices, replicate, tail, (:))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
  

graph :: Int -> Array Int
graph size = replicate (size*size) 0

createLineIndices :: Int -> Int -> Int -> Int -> Array Int 
createLineIndices x y y2 size = go [] y y2
    where 
        go arr currentY lastY | currentY == (lastY + 1) = arr 
                              | otherwise = go ((x*size + currentY) : arr) (currentY + 1) lastY

createColumnIndices :: Int -> Int -> Int -> Int -> Array Int 
createColumnIndices y x x2 size = go [] x x2
    where 
        go arr currentX lastX | currentX == (lastX + 1) = arr 
                              | otherwise = go ((y+size*currentX) : arr) (currentX + 1) lastX

createDiagonalIndices :: Int -> Int -> Int -> Int -> Int -> Array Int 
createDiagonalIndices x x2 y y2 size = go [] x y 
    where
        go arr currentX currentY | currentX == (x2 + 1) && currentY == (y2 + 1) = arr
                                 | otherwise = go ((currentY+size*currentX) : arr) (currentX + 1) (currentY + 1)

createDiagonalIndicesReversed :: Int -> Int -> Int -> Int -> Int -> Array Int 
createDiagonalIndicesReversed x x2 y y2 size = go [] x y 
    where
        go arr currentX currentY | currentX == (x2 + 1) && currentY == (y2 - 1) = arr
                                 | otherwise = go ((currentY+size*currentX) : arr) (currentX + 1) (currentY - 1)
 
createDiagonalIndicesReversedReversed :: Int -> Int -> Int -> Int -> Int -> Array Int 
createDiagonalIndicesReversedReversed x x2 y y2 size = go [] x y 
    where
        go arr currentX currentY | currentX == (x2 - 1) && currentY == (y2 + 1) = arr
                                 | otherwise = go ((currentY+size*currentX) : arr) (currentX - 1) (currentY + 1)
 

addLine :: Array Int -> Int -> Int -> Int -> Int -> Array Int 
addLine array x y y2 size | y < y2 = modifyAtIndices (createLineIndices x y y2 size)  (_ + 1) array 
                          | otherwise = modifyAtIndices (createLineIndices x y2 y size)  (_ + 1) array 
    
addColumn :: Array Int -> Int -> Int -> Int -> Int -> Array Int 
addColumn array y x x2 size | x < x2 = modifyAtIndices (createColumnIndices y x x2 size)  (_ + 1) array 
                            | otherwise = modifyAtIndices (createColumnIndices y x2 x size)  (_ + 1) array

addDiagonal :: Array Int -> Int -> Int -> Int -> Int -> Int -> Array Int 
addDiagonal array x x2 y y2 size = case x < x2, y < y2 of 
    true, true   ->  modifyAtIndices (createDiagonalIndices x x2 y y2 size) (_ + 1) array 
    false, false ->  modifyAtIndices (createDiagonalIndices x2 x y2 y size) (_ + 1) array
    true, false  ->  modifyAtIndices (createDiagonalIndicesReversed x x2 y y2 size) (_ + 1) array
    false, true  ->  modifyAtIndices (createDiagonalIndicesReversed x2 x y2 y size) (_ + 1) array

findMax :: Array Int -> Int 
findMax arr = foldl (\acc a -> if a > acc then a else acc) 0 arr  

countValues :: Array Int -> Int -> Int 
countValues arr val = foldl (\acc a -> if a >= 2 && a <= val then acc + 1 else acc) 0 arr  

part1 :: Array (Array (Array Int)) -> Array Int -> Int -> Int 
part1 data' grid size = case head data' of 
    (Just [[x,y],[x2,y2]]) -> case x == x2, y == y2 of 
        true, true -> part1 (fromMaybe [] $ tail data') (addLine grid x y y2 size) size
        true, false -> part1 (fromMaybe [] $ tail data') (addLine grid x y y2 size) size
        false, true -> part1 (fromMaybe [] $ tail data') (addColumn grid y x x2 size) size  
        false, false -> part1 (fromMaybe [] $ tail data') grid size   
    _ -> countValues grid (findMax grid) 

part1' :: Array (Array (Array Int)) -> Array Int -> Int -> Array Int 
part1' data' grid size = case head data' of 
    (Just [[y,x],[y2,x2]]) -> case x == x2, y == y2 of 
        true, true -> part1' (fromMaybe [] $ tail data') (addLine grid x y y2 size) size
        true, false -> part1' (fromMaybe [] $ tail data') (addLine grid x y y2 size) size
        false, true -> part1' (fromMaybe [] $ tail data') (addColumn grid y x x2 size) size  
        false, false -> part1' (fromMaybe [] $ tail data') grid size   
    _ -> grid

part2 :: Array (Array (Array Int)) -> Array Int -> Int -> Int 
part2 data' grid size = case head data' of 
    (Just [[x,y],[x2,y2]]) -> case x == x2, y == y2 of 
        true, true -> part2 (fromMaybe [] $ tail data') (addLine grid x y y2 size) size
        true, false -> part2 (fromMaybe [] $ tail data') (addLine grid x y y2 size) size
        false, true -> part2 (fromMaybe [] $ tail data') (addColumn grid y x x2 size) size  
        false, false -> part2 (fromMaybe [] $ tail data') (addDiagonal grid x x2 y y2 size) size    
    _ -> countValues grid (findMax grid) 

part2' :: Array (Array (Array Int)) -> Array Int -> Int -> Array Int 
part2' data' grid size = case head data' of 
    (Just [[y,x],[y2,x2]]) -> case x == x2, y == y2 of 
        true, true -> part2' (fromMaybe [] $ tail data') grid size
        true, false -> part2' (fromMaybe [] $ tail data') grid size
        false, true -> part2' (fromMaybe [] $ tail data') grid size  
        false, false -> part2' (fromMaybe [] $ tail data') (addDiagonal grid x x2 y y2 size) size    
    _ -> grid


main :: Effect Unit
main = do 
    content <- readTextFile UTF8 "src/Day5/input.txt"
    let data' = (\line -> (\pointCoords -> (\xy -> fromMaybe (-1) $ fromString xy) <$> (split (Pattern ",") pointCoords)) <$> (split (Pattern " -> ") line)) <$> lines content 
    logShow $ part1 data' (graph 1000) 1000 
    logShow $ part2 data' (graph 1000) 1000

-- this day has given me headaches