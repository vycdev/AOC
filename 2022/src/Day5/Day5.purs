module Day5
  ( main
  )
  where

import Prelude

import Data.Array (cons, deleteAt, foldl, head, index, intersectBy, last, length, mapWithIndex, snoc, tail, take, takeEnd)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..), replace, split)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day5/input.txt"

deleteAtIndex ∷ ∀ (t74 ∷ Type). Array t74 → Int → Array t74
deleteAtIndex arr index = fromMaybe [] $ deleteAt index arr

elementAtIndex ∷ Array Char → Int → Char
elementAtIndex arr i = fromMaybe ' ' $ index arr i

elementAtIndex' ∷ ∀ (t13 ∷ Type). Array (Array t13) → Int → Array t13
elementAtIndex' arr i = fromMaybe [] $ index arr i


getLastChars ∷ ∀ (f2 ∷ Type -> Type). Functor f2 ⇒ f2 (Array Char) → f2 Char
getLastChars chars = (\x ->fromMaybe ' ' $ last x) <$> chars

why ∷ ∀ (a5 ∷ Type). Array a5 → Array a5 → Array a5
why myArray1 myArray2 = foldl (\arr x -> arr `snoc` x) myArray1 myArray2

doMove :: Array (Array Char) -> Array Int -> Array (Array Char)
doMove chars [0, _, _] = chars  
doMove chars [amount, from, to] = 
    let lastChars = getLastChars chars
        newChars = mapWithIndex (\i x -> if i == (from - 1) then deleteAtIndex x (length x - 1) else x) chars
        newChars2 = mapWithIndex (\i x -> if i == (to - 1) then snoc x (elementAtIndex lastChars (from - 1)) else x) newChars
    in doMove newChars2 [amount-1, from, to]
     
doMove chars _ = chars  

doMove9001 :: Array (Array Char) -> Array Int -> Array (Array Char)
doMove9001 chars [amount, from, to] =
    let takenChars = takeEnd amount (elementAtIndex' chars (from-1))
        newChars = mapWithIndex (\i x -> if i == (from - 1) then take (length x - amount) x else x) chars
        newChars2 = mapWithIndex (\i x -> if i == (to - 1) then why x takenChars  else x) newChars
    in newChars2

doMove9001 chars _ = chars


main :: Effect Unit
main  = do 
    a <- input
    let setup = head $ split (Pattern "\r\n\r\n") a
    let setup2 = split (Pattern "\r\n") $ fromMaybe "" setup
    let setup3 = toCharArray <$> setup2

    let moves = last $ split (Pattern "\r\n\r\n") a
    let moves2 = split (Pattern "\r\n") $ fromMaybe "" moves 
    let moves3 = replace (Pattern "move ") (Replacement "") <$> moves2
    let moves4 = replace (Pattern " from") (Replacement "") <$> moves3
    let moves5 = replace (Pattern " to") (Replacement "") <$> moves4
    let moves6 = split (Pattern " ") <$> moves5 
    let moves7 = (\x -> fromMaybe 0 <$> fromString <$> x) <$> moves6


    let solution1 = fromCharArray $ getLastChars $ foldl doMove setup3 moves7
    let solution2 = fromCharArray $ getLastChars $ foldl doMove9001 setup3 moves7
    logShow $ solution1
    logShow $ solution2