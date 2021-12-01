module Day1 where

import Prelude
import Data.Array (mapMaybe)
import Data.Int (fromString)
import Data.List (List(..), fromFoldable, (:))
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day1/input.txt"

-- countChanges (199 : 200 : 208 : 210 : 200 : 207 : 240 : 269 : 260 : 263 : Nil) 
-- go (>199< : >200< : 208 : 210 : 200 : 207 : 240 : 269 : 260 : 263 : Nil) 0
-- go (>200< : >208< : 210 : 200 : 207 : 240 : 269 : 260 : 263 : Nil) 1
-- go (>208< : >210< : 200 : 207 : 240 : 269 : 260 : 263 : Nil) 2
-- go (>210< : >200< : 207 : 240 : 269 : 260 : 263 : Nil) 3
-- go (>200< : >207< : 240 : 269 : 260 : 263 : Nil) 3
-- go (>207< : >240< : 269 : 260 : 263 : Nil) 4
-- go (>240< : >269< : 260 : 263 : Nil) 5
-- go (>269< : >260< : 263 : Nil) 6
-- go (>260< : >263< : Nil) 6
-- go (263 : Nil) 7
-- go >Nil< 7
-- 7
countChanges :: List Int -> Int
countChanges list = go list 0
  where
  go :: List Int -> Int -> Int
  go l acc = case l of
    (x : y : xs)
      | y > x -> go (y : xs) (acc + 1)
      | y < x -> go (y : xs) acc
      | otherwise -> go (y : xs) acc
    _ -> acc

-- countChanges2 (199 : 200 : 208 : 210 : 200 : 207 : 240 : 269 : 260 : 263 : Nil) 
-- go (>199< : 200 : 208 : >210< : 200 : 207 : 240 : 269 : 260 : 263 : Nil) 0
-- go (>200< : 208 : 210 : >200< : 207 : 240 : 269 : 260 : 263 : Nil) 1
-- go (>208< : 210 : 200 : >207< : 240 : 269 : 260 : 263 : Nil) 1
-- go (>210< : 200 : 207 : >240< : 269 : 260 : 263 : Nil) 1
-- go (>200< : 207 : 240 : >269< : 260 : 263 : Nil) 2
-- go (>207< : 240 : 269 : >260< : 263 : Nil) 3
-- go (>240< : 269 : 260 : >263< : Nil) 4
-- go (269 : 260 : 263 : Nil) 5
-- go (260 : 263 : Nil) 5
-- go (263 : Nil) 5
-- go Nil 5
-- 5
countChanges2 :: List Int -> Int
countChanges2 list = go list 0
  where
  go :: List Int -> Int -> Int
  go l acc = case l of
    (a : b : c : d : xs)
      | d > a -> go (b : c : d : xs) (acc + 1)
      | d < a -> go (b : c : d : xs) acc
      | otherwise -> go (b : c : d : xs) acc
    _ -> acc

-- tails (1 : 2 : 3 : 4 : 5 : Nil)
-- ((1 : 2 : 3 : 4 : 5 : Nil) : (2 : 3 : 4 : 5 : Nil) : (3 : 4 : 5 : Nil) : (4 : 5 : Nil) : (5 : Nil) : Nil)
tails :: forall a. List a -> List (List a)
tails Nil = Nil
tails as'@(Cons _ as) = as' : tails as

-- solution2helper (199 : 200 : 208 : 210 : 200 : 207 : 240 : 269 : 260 : 263 : Nil) 
-- (607 : 618 : 618 : 617 : 647 : 716 : 769 : 792 : Nil)
-- It basically adds every 3 first numbers together into another list 
solution2helper :: List Int -> List Int
solution2helper list = join $ map foo (tails list)
  where
  foo (a : b : c : _) = ((a + b + c) : Nil)
  foo _ = Nil

main :: Effect Unit
main = do
  content <- input
  let
    something = mapMaybe fromString (lines content)
  logShow $ countChanges $ fromFoldable something
  logShow $ countChanges $ solution2helper (fromFoldable something)
  logShow $ countChanges2 $ fromFoldable something
