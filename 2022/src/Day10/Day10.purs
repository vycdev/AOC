module Day10 where

import Prelude

import Data.Array (concat, drop, foldl, last, snoc, take, (:))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day10/input.txt"

data Signal = Noop | Addx Int

instance Show Signal where
    show a = case a of
        Noop -> "Noop" 
        Addx v -> "Addx" <> show v

parseSignal :: String -> Maybe Signal
parseSignal s = case split (Pattern " ") s of 
    ["noop"] -> Just Noop
    ["addx", v] -> Just (Addx (fromMaybe 0 $ fromString v))
    _ -> Nothing

signalsToCycles :: Array Signal -> Array Int
signalsToCycles a = foldl (\acc x -> concat [acc, transform x] ) [1] a
    where
      transform t = case t of
        Noop -> [0]
        Addx v -> [0, v] 

isSolution1Index :: Int -> Boolean
isSolution1Index i 
                | i == 20 = true
                | i == 60 = true 
                | i == 100 = true 
                | i == 140 = true 
                | i == 180 = true
                | i == 220 = true 
                | otherwise = false

chunksOf :: Int -> Array String -> Array (Array String)
chunksOf _ [] = []
chunksOf s xs = (take s xs) : chunksOf s (drop s xs)

main :: Effect Unit
main = do 
    amogus <- input
    let lines = split (Pattern "\r\n") amogus
    let signals = fromMaybe Noop <$> (parseSignal <$> lines)
    let cycles = signalsToCycles signals
    let cycleValues = foldl (\acc x -> snoc acc ((fromMaybe 0 $ last acc) + x)) [] cycles

    let solution1 = foldlWithIndex (\i acc x -> if isSolution1Index $ i+1 then acc + (i+1)*x else acc) 0 cycleValues
    let solution2 = joinWith "\n" $ joinWith "" <$> (chunksOf 40 $ foldlWithIndex (\i acc x -> if (x == (i `mod` 40) || (x+1) == (i `mod` 40) || (x-1) == (i `mod` 40)) then snoc acc "⚪" else snoc acc "⚫") [] cycleValues)

    logShow signals
    logShow cycleValues
    logShow solution1
    log solution2