module Day2
  ( input
  , main
  , solution1
  , solution2
  )
  where

import Prelude

import Control.Monad.List.Trans (mapMaybe)
import Data.Array (tail)
import Data.Int (fromString)
import Data.List (List, Pattern(..), fromFoldable, (:))
import Data.List.NonEmpty (head)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..))
import Data.String.Utils (lines, words)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
  
input :: Effect String
input = readTextFile UTF8 "src/Day2/input.txt"


-- forward 5
-- down 5
-- forward 8
-- up 3
-- down 8
-- forward 2
-- [("forward":"5":Nil)...]

solution1 :: List (List String) -> Int 
solution1 arr = go arr 0 0
    where
    go :: List (List String) -> Int -> Int -> Int 
    go (("forward":distance:_):xs) h d = go xs (h + (fromMaybe 0 $ fromString distance)) d 
    go (("up":distance:_):xs) h d = go xs h (d - (fromMaybe 0 $ fromString distance))
    go (("down":distance:_):xs) h d = go xs h (d + (fromMaybe 0 $ fromString distance))
    go _ h d = h*d 

solution2 :: List (List String) -> Int 
solution2 arr = go arr 0 0 0
    where
    go :: List (List String) -> Int -> Int -> Int -> Int 
    go (("forward":distance:_):xs) h d a = go xs (h + (fromMaybe 0 $ fromString distance)) (d + a*(fromMaybe 0 $ fromString distance))  a
    go (("up":distance:_):xs) h d a = go xs h d (a - (fromMaybe 0 $ fromString distance))
    go (("down":distance:_):xs) h d a = go xs h d (a + (fromMaybe 0 $ fromString distance))
    go _ h d _ = h*d    

main :: Effect Unit
main = do 
    content <- input
    let list = (\a -> fromFoldable $ words a) <$> (fromFoldable $ lines content)
    logShow $ solution1 list
    logShow $ solution2 list