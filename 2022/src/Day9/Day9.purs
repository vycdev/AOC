module Day9
  (
    main
  )
  where

import Prelude

import Data.Array (concat, foldl, last, length, nub, replicate, snoc)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day9/input.txt"

data Move = L Int | R Int | U Int | D Int

instance Show Move where
    show a = case a of
        L v -> "L" <> show v 
        R v -> "R" <> show v 
        U v -> "U" <> show v 
        D v -> "D" <> show v 

parseMove :: String -> Maybe Move
parseMove s = case split (Pattern " ") s of 
    ["L", v] -> Just $ L (fromMaybe 0 $ fromString v)
    ["R", v] -> Just $ R (fromMaybe 0 $ fromString v)
    ["U", v] -> Just $ U (fromMaybe 0 $ fromString v)
    ["D", v] -> Just $ D (fromMaybe 0 $ fromString v)
    _ -> Nothing

executeMove :: Move -> (Tuple Int Int) -> (Tuple Int Int)
executeMove m (Tuple hx hy) = case m of 
    (L _) -> Tuple (hx-1) hy 
    (R _) -> Tuple (hx+1) hy
    (U _) -> Tuple hx (hy+1)
    (D _) -> Tuple hx (hy-1)

getTPosition :: (Tuple Int Int) -> (Tuple Int Int) -> (Tuple Int Int)
getTPosition h t = 
    let 
        (Tuple hx hy) = h
        (Tuple tx ty) = t
    in
      if hx == tx && hy == ty then t else           -- h over it 
      if hx + 1 == tx && hy == ty then t else        -- h on the left 
      if hx - 1 == tx && hy == ty then t else        -- h on the right
      if hx == tx && hy + 1 == ty then t else        -- h on the bottom 
      if hx == tx && hy - 1 == ty then t else        -- h on the top
      if hx + 1 == tx && hy + 1 == ty then t else     -- h on the bottom left
      if hx - 1 == tx && hy - 1 == ty then t else     -- h on the top right 
      if hx + 1 == tx && hy - 1 == ty then t else     -- h on the top left
      if hx - 1 == tx && hy + 1 == ty then t else     -- h on the bottom right 
      
      if hx + 2 == tx && hy + 0 == ty then (Tuple (tx - 1) ty) else
      if hx - 2 == tx && hy - 0 == ty then (Tuple (tx + 1) ty) else

      if hx == tx && hy + 2 == ty then (Tuple tx (ty - 1)) else
      if hx == tx && hy - 2 == ty then (Tuple tx (ty + 1)) else

      if hx + 2 == tx && hy + 1 == ty then (Tuple (tx - 1) (ty - 1)) else
      if hx + 2 == tx && hy - 1 == ty then (Tuple (tx - 1) (ty + 1)) else

      if hx - 2 == tx && hy - 1 == ty then (Tuple (tx + 1) (ty + 1)) else
      if hx - 2 == tx && hy + 1 == ty then (Tuple (tx + 1) (ty - 1)) else

      if hx + 1 == tx && hy + 2 == ty then (Tuple (tx - 1) (ty - 1)) else
      if hx - 1 == tx && hy + 2 == ty then (Tuple (tx + 1) (ty - 1)) else

      if hx + 1 == tx && hy - 2 == ty then (Tuple (tx - 1) (ty + 1)) else
      if hx - 1 == tx && hy - 2 == ty then (Tuple (tx + 1) (ty + 1)) else 
      
      if hx + 2 == tx && hy + 2 == ty then (Tuple (tx - 1) (ty - 1)) else 
      if hx + 2 == tx && hy - 2 == ty then (Tuple (tx - 1) (ty + 1)) else 

      if hx - 2 == tx && hy + 2 == ty then (Tuple (tx + 1) (ty - 1)) else 
      if hx - 2 == tx && hy - 2 == ty then (Tuple (tx + 1) (ty + 1)) else 
      (Tuple 0 0)



oneMoveToMultpleOneMoves :: Maybe Move -> Array (Move)
oneMoveToMultpleOneMoves m = case m of 
    Just (L v) -> replicate v (L 1)
    Just (R v) -> replicate v (R 1)
    Just (U v) -> replicate v (U 1)
    Just (D v) -> replicate v (D 1)
    _ -> []

main :: Effect Unit
main = do 
    a <- input
    let splitted = split (Pattern "\r\n") a
    let moves = parseMove <$> splitted
    let oneStepMoves = concat $ oneMoveToMultpleOneMoves <$> moves

    let hPositions = foldl (\acc x -> snoc acc $ executeMove x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] oneStepMoves
    let tPositions1 = foldl (\acc x -> snoc acc $ getTPosition x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] hPositions

    -- I'm about to do a pro gamer move
    let tPositions2 = foldl (\acc x -> snoc acc $ getTPosition x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] tPositions1
    let tPositions3 = foldl (\acc x -> snoc acc $ getTPosition x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] tPositions2
    let tPositions4 = foldl (\acc x -> snoc acc $ getTPosition x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] tPositions3
    let tPositions5 = foldl (\acc x -> snoc acc $ getTPosition x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] tPositions4
    let tPositions6 = foldl (\acc x -> snoc acc $ getTPosition x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] tPositions5
    let tPositions7 = foldl (\acc x -> snoc acc $ getTPosition x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] tPositions6
    let tPositions8 = foldl (\acc x -> snoc acc $ getTPosition x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] tPositions7
    let tPositions9 = foldl (\acc x -> snoc acc $ getTPosition x (fromMaybe (Tuple 0 0) $ last acc)) [(Tuple 0 0)] tPositions8

-- ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⣠⣤⣤⣤⠴⠶⠶⠶⠶⠒⠾⠿⠿⠿⣛⡛⠛⠛⠛⠛⠛⠻⠿⡷⠶⠶⢶⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀
-- ⠀⠀⠀⠀⠀⠀⢀⣤⡾⠟⠛⠉⣉⣩⣤⡴⠦⠭⠥⠒⠒⠒⠒⠒⠒⠒⠒⠂⠤⠀⢀⣀⠈⠑⠢⢀⠑⠀⠀⠙⢿⣄⠀⠀⠀⠀⠀⠀⠀⠀
-- ⠀⠀⠀⠀⣠⡾⠟⠁⣠⡢⠔⢫⠞⣉⣀⡀⠀⠀⠀⠐⠒⠄⠠⠀⠀⠐⡠⢂⡴⠶⠦⢴⡊⠙⠒⠀⠑⠀⠀⠀⠀⠹⣧⡀⠀⠀⠀⠀⠀⠀
-- ⠀⠀⠀⢠⣿⠀⡠⢊⡫⡀⢀⣤⣞⣡⣼⣿⣦⠀⠐⠉⠱⡤⢢⠦⠀⠀⣰⠋⣀⣤⣴⣿⣿⣆⠀⠀⠀⠀⠀⠀⠙⠳⢾⣷⡀⠀⠀⠀⠀⠀
-- ⠀⠀⠀⣼⡏⣰⠁⠠⠪⠿⣟⠩⠉⠀⠀⠈⢻⡧⠄⣴⠞⠁⣣⠖⠀⢰⣧⠞⠁⠀⠠⠍⡻⣼⡆⠀⢀⣀⡀⠀⠀⠀⠀⠙⣧⡀⠀⠀⠀⠀
-- ⠀⣴⡾⠟⣽⢋⡒⠦⡢⠐⠀⠄⠒⠲⠶⠖⠋⠀⢸⡇⠀⠀⠙⠀⠀⠘⣷⡀⠤⠤⠀⠀⠀⠉⠛⠻⡍⠀⠐⠉⣉⣗⠦⣄⠘⢿⣦⡀⠀⠀
-- ⣾⠋⠀⢸⠇⢹⠟⢦⣄⡀⠄⠀⠀⠉⠁⣰⠶⢖⣾⠁⠀⠀⠀⠐⠒⢦⣤⣝⠓⠒⠒⠊⠀⠈⠀⠀⢀⣴⠞⠋⣽⢻⠱⡈⢳⡈⢯⠻⣦⠀
-- ⣿⠀⡆⠸⣆⢸⡦⡄⠉⠛⠶⣬⣔⡀⠘⠁⢸⡏⠁⠀⠀⠶⢦⣤⡀⠈⡇⠈⠳⠄⠀⢀⠀⠀⣀⡴⢿⠥⣄⣼⠃⡌⠀⢳⠀⢳⠸⡄⠘⣧
-- ⣿⡀⡇⠀⠈⠷⣇⡗⣦⣠⡀⠈⠙⠛⡿⠶⠾⢿⣶⣶⣶⣶⣀⣀⣁⣀⣁⣀⣠⣤⣿⠿⠟⠛⣉⣀⡏⢀⡿⠁⠰⠀⠀⢸⠀⠀⠀⡇⠀⣿
-- ⠘⣷⡁⢀⢸⠀⣿⠀⡟⠀⣷⠋⢳⡾⠙⢷⡀⠀⣠⠤⣌⠉⠉⣉⣭⣍⠉⣩⠭⢤⣀⡴⠚⢲⡇⠀⣿⠏⠀⠠⠃⠀⠀⣸⠀⠀⠀⠁⣼⠏
-- ⠀⠘⣷⢸⠈⡆⣿⣿⣁⢀⠏⠀⢸⡇⠀⠀⢻⣾⠁⠀⠈⢳⣴⠏⠀⠹⣶⠇⠀⠀⢹⡀⣀⣼⣷⡾⠃⢠⠀⢀⠄⠀⠠⠁⠀⠀⣀⣼⠋⠀
-- ⠀⠀⢸⣿⠀⡇⣿⣿⣿⣿⣤⣄⣼⠃⠀⠀⢸⡟⠀⠀⠀⠀⣿⠀⠀⠀⣿⡀⠀⢀⣿⣿⣿⣿⡟⠁⢠⠃⠀⠀⠀⡀⠀⠀⢀⣼⠟⠁⠀⠀
-- ⠀⠀⢸⣿⠀⡇⣿⣿⣿⣿⣿⣿⣿⣷⣶⣦⣿⣧⣀⣀⣤⣤⣿⣶⣶⣶⣿⣿⣿⣿⣿⣿⡿⣫⠄⢀⠂⠀⠀⠀⠀⡄⠀⢠⣿⠁⠀⠀⠀⠀
-- ⠀⠀⢸⣿⠀⣧⣿⠻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⣩⠞⠁⡰⠁⠀⠠⠀⠀⡐⠀⢠⡾⠃⠀⠀⠀⠀⠀
-- ⠀⠀⢸⡇⠀⣿⡟⢀⡟⠀⣿⠋⢻⡿⠻⣿⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⢁⡔⠁⠠⠞⠀⠀⠀⠁⢀⠌⢀⣴⠟⠁⠀⠀⠀⠀⠀⠀
-- ⠀⠀⣼⠃⡄⢹⣿⡙⢇⣠⡇⠀⣸⠁⢠⠇⠀⢹⠃⢠⠛⠙⡏⠉⣇⣼⠿⢃⡴⠋⠀⠐⠁⠔⠀⠐⠁⣠⣢⣴⠟⠁⠀⠀⠀⠀⠀⠀⠀⠀
-- ⠀⠀⣿⠀⡇⠸⡿⢷⣄⡀⠙⠒⠳⡤⠼⣄⣀⢼⣀⢾⣀⣸⣶⡾⠟⣁⡴⠋⢀⡠⠒⠁⠀⠀⢀⣤⡾⠟⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
-- ⠀⠀⣿⠀⠻⡄⠉⠠⡉⠙⠳⠶⣶⣶⣶⣾⣷⣶⠿⠿⠟⠋⠉⠖⠫⠕⠒⠈⠀⢀⣤⣴⡶⠟⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
-- ⠀⠀⢿⡄⠀⠉⠓⠀⠀⠈⠉⠠⠌⠀⠀⠀⣀⠠⠄⠂⠠⠤⠤⠴⠊⠁⣀⣴⡾⠛⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
-- ⠀⠀⠈⠻⣦⣑⠒⠤⣅⣀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣤⣤⣶⠶⠶⠛⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
-- ⠀⠀⠀⠀⠈⠙⠛⠶⠶⣤⣭⣭⣭⣭⣴⠶⠶⠛⠛⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀




    let solution1 = length $ nub tPositions1
    let solution2 = length $ nub tPositions9

    logShow $ solution1
    logShow $ solution2
