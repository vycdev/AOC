module Day4 where

import Prelude

import Data.Array (deleteAt, foldl, head, index, length, reverse, tail)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, trim)
import Data.String.Utils (lines, words)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- Array Int 

-- Array Array Int 
-- Array Array Int 
-- Array Array Int 
-- Array Array Int 
-- Array Array Int 
-- Array Array Int 


parseArrayWithFromString :: Array String -> Array Int
parseArrayWithFromString arr = (\a -> fromMaybe (-999) $ fromString a) <$> arr

parseStringOfNumbersWithPattern :: String -> Pattern -> Array Int 
parseStringOfNumbersWithPattern string pattern = parseArrayWithFromString $ split pattern string

parseStringOfNumbersWithWords :: String -> Array Int 
parseStringOfNumbersWithWords string = parseArrayWithFromString $ words $ trim string
  
data Mark = Marked Int | Unmarked Int 

type Board a = Array (Array a)

unmarkEverythingFromBoard :: Board Int -> Board Mark  
unmarkEverythingFromBoard b = (\line -> (\column -> Unmarked column) <$> line) <$> b

-- flipMark :: Mark -> Mark 
-- flipMark (Marked foo) = Unmarked foo
-- flipMark (Unmarked foo) = Marked foo 


isMarked :: Mark -> Boolean 
isMarked (Marked _) = true 
isMarked (Unmarked _) = false 

markMatchingInts :: Board Mark -> Int -> Board Mark 
markMatchingInts b v = (\line -> (\column -> mark column) <$> line) <$> b
    where 
    mark :: Mark -> Mark 
    mark (Unmarked c) | c == v = Marked c 
                      | otherwise = Unmarked c  
    mark c = c

markMatchingIntsForAllBoards :: Array (Board Mark) -> Int -> Array (Board Mark) 
markMatchingIntsForAllBoards b v = ((\board -> markMatchingInts board v) <$> b)


-- IM SORRY FOR WHAT IM ABOUT TO WRITE
checkDiagonals :: Board Mark -> Boolean 
checkDiagonals [[Marked _,_,_,_,_],
                [_,Marked _,_,_,_],
                [_,_,Marked _,_,_],
                [_,_,_,Marked _,_],
                [_,_,_,_,Marked _]] = true

checkDiagonals [[_,_,_,_,Marked _],
                [_,_,_,Marked _,_],
                [_,_,Marked _,_,_],
                [_,Marked _,_,_,_],
                [Marked _,_,_,_,_]] = true

checkDiagonals _ = false
-- I'm sorry for burning your eyes  
-- AND I DONT EVEN NEED IT OMG
-- Im gonna keep it here though so you can laugh at it
-- And I think Im going to do it again 


transposeMatrix :: Board Mark -> Board Mark 
transposeMatrix [[a,b,c,d,e],
                 [f,g,h,i,j],
                 [k,l,m,n,o],
                 [p,q,r,s,t],
                 [u,v,w,x,y]] = [[a,f,k,p,u],
                                 [b,g,l,q,v],
                                 [c,h,m,r,w],
                                 [d,i,n,s,x],
                                 [e,j,o,t,y]]
transposeMatrix b = b  
-- I'm sorry 
checkBingo :: Board Mark -> Boolean 
checkBingo board = (checkLines board) || (checkLines $ transposeMatrix board) 
    where 
    checkLines b = foldl (\acc v -> v || acc) false $ (\line -> foldl (\acc v -> acc && (isMarked v)) true line) <$> b

sumUnchecked :: Board Mark -> Int 
sumUnchecked board = foldl (+) 0 $ (\lines -> foldl (\acc v -> acc + (imTired v)) 0 lines) <$> board 
    where 
    imTired (Unmarked a) = a 
    imTired (Marked _) = 0


score :: Array (Board Mark) -> Int -> Maybe Int
score boards val = go 0 
    where
      go i = case index boards i of 
        (Just board) -> case checkBingo board of
                true -> Just (val * (sumUnchecked board))
                false -> go (i+1)  
        _ -> Nothing   

indexOfScore :: Array (Board Mark) -> Maybe Int
indexOfScore boards = go 0 
    where
      go i = case index boards i of 
        (Just board) -> case checkBingo board of
                true -> Just i
                false -> go (i+1)  
        _ -> Nothing   

solution1 :: Array (Board Mark) -> Array Int -> Int
solution1 boards arr = go (markMatchingIntsForAllBoards boards (fromMaybe (-1) $ head arr)) arr 
    where 
    go :: Array (Board Mark) -> Array Int -> Int 
    go b a = case (head a) of 
        (Just v) -> case score (markMatchingIntsForAllBoards b v) v of 
            (Just val) -> val
            _ -> case tail a of 
                (Just t) -> go (markMatchingIntsForAllBoards b v) t
                _ -> -2             
        _ -> -1 

getRidOfAllWinners :: Array (Board Mark) -> Array (Board Mark)
getRidOfAllWinners boards = case indexOfScore boards of
    (Just indx) -> getRidOfAllWinners (fromMaybe [] $ deleteAt indx boards)
    _ -> boards 

solution2 :: Array (Board Mark) -> Array Int -> Int
solution2 boards arr = go (markMatchingIntsForAllBoards boards (fromMaybe (-1) $ head arr)) arr (-1) 
    where 
    go :: Array (Board Mark) -> Array Int -> Int -> Int 
    go [] _ sc = sc 
    go b a sc = case (head a), (tail a) of 
        (Just v), (Just t) -> 
            go markedLosers t (fromMaybe sc $ score markedBoards v)
            where 
                markedBoards = markMatchingIntsForAllBoards b v
                markedLosers = getRidOfAllWinners markedBoards
                    
        (Just v), _ -> go markedLosers [] (fromMaybe sc $ score markedBoards v)
            where
                markedBoards = markMatchingIntsForAllBoards b v
                markedLosers = getRidOfAllWinners markedBoards           
        _, _ -> sc

parseBoard :: String -> Board Int
parseBoard str = (\a -> parseStringOfNumbersWithWords a) <$> lines str

parseBoards :: Array String -> Array (Board Int)
parseBoards array = parseBoard <$> array    
    
emptyLines :: String -> Array String 
emptyLines str = split (Pattern "\r\n\r\n") str 
main:: Effect Unit
main = do 
    content <- readTextFile UTF8 "src/Day4/input.txt"
    let sequenceAndBoards = emptyLines content 
    let sequence = parseStringOfNumbersWithPattern (fromMaybe "" $ head sequenceAndBoards) $ Pattern ","
    let boards = parseBoards $ (fromMaybe [] $ tail sequenceAndBoards)
    -- logShow sequence 
    -- logShow unparsedBoards 
    -- logShow sequence
    logShow $ solution1 (unmarkEverythingFromBoard <$> boards) sequence
    logShow $ solution2 (unmarkEverythingFromBoard <$> boards) sequence

    