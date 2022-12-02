module Day2
  ( main
  )
  where

import Prelude

import Data.Foldable (sum)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day2/input.txt"

format :: Effect String -> Effect (Array (Array String))
format str = do
    first <- str
    let arr = split (Pattern "\r\n") first
    let arr2 = split (Pattern " ") <$> arr
    pure arr2

-- (1 for Rock, 2 for Paper, and 3 for Scissors) 
-- (0 if you lost, 3 if the round was a draw, and 6 if you won).
calculatePoints1 :: Array String -> Int
calculatePoints1 arr = case arr of 
     ["A", "X"] -> 1 + 3
     ["A", "Y"] -> 2 + 6
     ["A", "Z"] -> 3 + 0
     ["B", "X"] -> 1 + 0
     ["B", "Y"] -> 2 + 3
     ["B", "Z"] -> 3 + 6
     ["C", "X"] -> 1 + 6
     ["C", "Y"] -> 2 + 0
     ["C", "Z"] -> 3 + 3
     _ -> 0


-- X means you need to lose, 
-- Y means you need to end the round in a draw, 
-- and Z means you need to win.
calculatePoints2 :: Array String -> Int
calculatePoints2 arr = case arr of 
     ["A", "X"] -> 3 + 0
     ["A", "Y"] -> 1 + 3
     ["A", "Z"] -> 2 + 6
     ["B", "X"] -> 1 + 0
     ["B", "Y"] -> 2 + 3
     ["B", "Z"] -> 3 + 6
     ["C", "X"] -> 2 + 0
     ["C", "Y"] -> 3 + 3
     ["C", "Z"] -> 1 + 6
     _ -> 0

solve :: Effect (Array (Array String)) -> (Array String -> Int) -> Effect (Int)
solve a calc = do 
    arr <- a
    pure $ sum (calc <$> arr)
     


main ∷ Effect Unit
main = do 
    solution1 <- solve (format input) calculatePoints1
    solution2 <- solve (format input) calculatePoints2
    logShow $ solution1
    logShow $ solution2