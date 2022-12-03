module Day3
  ( main
  )
  where

import Prelude

import Data.Array (drop, elem, find, intersect, take , (:))
import Data.Char (toCharCode)
import Data.CodePoint.Unicode (isLower)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), codePointFromChar, split, splitAt, toLower, toUpper)
import Data.String as STR
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Unsafe (char)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

input :: Effect String
input = readTextFile UTF8 "src/Day3/input.txt"


findDuplicate':: String -> String -> Maybe Char
findDuplicate' a b = find (\x -> elem x (toCharArray b)) (toCharArray a)

findDuplicate :: { after :: String, before :: String } -> Maybe Char
findDuplicate {after, before} = findDuplicate' after before

findDuplicateForChunksOf3 :: Array String -> Maybe Char 
findDuplicateForChunksOf3 [a,b,c] = find (\x -> elem x (toCharArray a `intersect` toCharArray b)) (toCharArray c)
findDuplicateForChunksOf3 _ = Nothing
                
chunksOf3 :: Array String -> Array (Array String)
chunksOf3 [] = []
chunksOf3 xs = (take 3 xs) : chunksOf3 (drop 3 xs)

reverseCase :: Char -> Char
reverseCase c = if isLower (codePointFromChar c) then char $ toUpper (singleton c) else char $ toLower (singleton c) 

fixNumber :: Int -> Int
fixNumber n = if n > 90 then n - 6 else n
charToNumber :: Maybe Char -> Int
charToNumber (Just c) = fixNumber (toCharCode (reverseCase c)) - 64
charToNumber _ = 0

main :: Effect Unit
main = do
    arr <- input 
    let lines = split (Pattern "\r\n") arr
    let formattedHalfes = (\a -> splitAt ((STR.length a)/2) a) <$> lines 
    let chars = findDuplicate <$> formattedHalfes

    let solution1 = sum $ charToNumber <$> chars

    logShow $ solution1

    let formattedGroups = chunksOf3 lines 
    let commonGroupLetters = findDuplicateForChunksOf3 <$> formattedGroups
    let solution2 = sum $ charToNumber <$> commonGroupLetters
    
    logShow $ solution2


