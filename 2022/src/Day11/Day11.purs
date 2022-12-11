module Day11
  ( 
   main
  )
  where

import Prelude

import Data.Array (foldl, length, modifyAt, range, replicate, snoc, sort, tail, takeEnd, updateAt, zipWith, (!!))
import Data.BigInt (BigInt, fromInt)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, stripPrefix)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day11/input.txt"

parseStartingItems :: String -> Array Int 
parseStartingItems s = (\x -> fromMaybe 0 $ fromString x)<$> (split (Pattern ", ") $ fromMaybe "" $ stripPrefix (Pattern "  Starting items: ") s) 

operationExecuter :: Int -> Array String -> Int
operationExecuter old [s1, o, s2] = case o of 
    "*" -> case s1, s2 of 
        "old", "old" -> old * old 
        "old", v -> old * (fromMaybe 0 $ fromString v)  
        v, "old" -> old * (fromMaybe 0 $ fromString v)  
        _, _ -> 0
    "+" -> case s1, s2 of 
        "old", "old" -> old + old 
        "old", v -> old + (fromMaybe 0 $ fromString v)  
        v, "old" -> old + (fromMaybe 0 $ fromString v)  
        _, _ -> 0
    _ -> 0
operationExecuter _ _ = 0 

operationExecuter' :: BigInt -> Array String -> BigInt
operationExecuter' old [s1, o, s2] = case o of 
    "*" -> case s1, s2 of 
        "old", "old" -> old * old 
        "old", v -> old * fromInt (fromMaybe 0 $ fromString v)  
        v, "old" -> old * fromInt (fromMaybe 0 $ fromString v)  
        _, _ -> fromInt 0
    "+" -> case s1, s2 of 
        "old", "old" -> old + old 
        "old", v -> old + fromInt (fromMaybe 0 $ fromString v)  
        v, "old" -> old + fromInt (fromMaybe 0 $ fromString v)  
        _, _ -> fromInt 0
    _ -> fromInt 0
operationExecuter' _ _ = fromInt 0 


parseOperation :: String -> Array String
parseOperation s = split (Pattern " ") $ fromMaybe "" $ stripPrefix (Pattern "  Operation: new = ") s 

parseTest :: String -> Int 
parseTest s = fromMaybe 0 $ fromString $ fromMaybe "" $ stripPrefix (Pattern "  Test: divisible by ") s

parseIfTrue :: String -> Int 
parseIfTrue s = fromMaybe 0 $ fromString $ fromMaybe "" $ stripPrefix (Pattern "    If true: throw to monkey ") s

parseIfFalse :: String -> Int 
parseIfFalse s = fromMaybe 0 $ fromString $ fromMaybe "" $ stripPrefix (Pattern "    If false: throw to monkey ") s

executeRound :: Array (Array Int) -> Array (Array String) -> Array Int -> Array Int -> Array Int -> Array (Array Int) -- amazing type isnt it
executeRound invs ops tests trs fls = ohgod invs 0 
    where
      ohgod :: Array (Array Int) -> Int -> Array (Array Int)
      ohgod a i 
            | i == (length a) = a 
            | otherwise = ohgod newInvs (i+1) 
        where 

            newInvs = fromMaybe [] $ updateAt i [] $ foldl (\acc (Tuple indx val) -> fromMaybe [] (modifyAt indx (\x -> snoc x val) acc)) a updates 
            
            updates :: Array (Tuple Int Int)
            updates = foldl (\acc x -> snoc acc (Tuple (whereToUpdate x) (newWorry x))) [] (fromMaybe [] $ a !! i)

            whereToUpdate v = if (newWorry v) `mod` (fromMaybe 1 $ tests !! i) == 0 then fromMaybe 0 $ trs !! i else fromMaybe 0 $ fls !! i 

            newWorry v = (operationExecuter v (fromMaybe ["old","*","1"] $ ops !! i)) / 3 -- v is old value i is current monkey
            -- newWorry v = (operationExecuter v (fromMaybe ["old","*","1"] $ ops !! i)) `mod` ohTheMisery -- v is old value i is current monkey
            -- ohTheMisery = foldl (\acc x -> acc * x ) 1 tests 


numberOfInspectsInRound :: Array (Array Int) -> Array (Array String) -> Array Int -> Array Int -> Array Int -> Array Int
numberOfInspectsInRound invs ops tests trs fls = ohgod invs 0 (replicate (length invs) 0) 
    where
      ohgod :: Array (Array Int) -> Int -> Array Int -> Array Int
      ohgod a i inspects 
            | i == (length a) = inspects
            | otherwise = ohgod newInvs (i+1) newInspects
        where 
            newInspects = fromMaybe [] $ modifyAt i (\x -> x + (length $ fromMaybe [] $ a !! i)) inspects

            newInvs = fromMaybe [] $ updateAt i [] $ foldl (\acc (Tuple indx val) -> fromMaybe [] (modifyAt indx (\x -> snoc x val) acc)) a updates 

            updates :: Array (Tuple Int Int)
            updates = foldl (\acc x -> snoc acc (Tuple (whereToUpdate x) (newWorry x))) [] (fromMaybe [] $ a !! i)

            whereToUpdate v = if (newWorry v) `mod` (fromMaybe 1 $ tests !! i) == 0 then fromMaybe 0 $ trs !! i else fromMaybe 0 $ fls !! i 

            newWorry v = (operationExecuter v (fromMaybe ["old","*","1"] $ ops !! i)) / 3 -- v is old value i is current monkey
            -- newWorry v = (operationExecuter v (fromMaybe ["old","*","1"] $ ops !! i)) `mod` ohTheMisery -- v is old value i is current monkey
            -- ohTheMisery = foldl (\acc x -> acc * x ) 1 tests 

            


executeRound' :: Array (Array BigInt) -> Array (Array String) -> Array Int -> Array Int -> Array Int -> Array (Array BigInt) -- amazing type isnt it
executeRound' invs ops tests trs fls = ohgod invs 0 
    where
      ohgod :: Array (Array BigInt) -> Int -> Array (Array BigInt)
      ohgod a i 
            | i == (length a) = a 
            | otherwise = ohgod newInvs (i+1) 
        where 
            newInvs = fromMaybe [] $ updateAt i [] $ foldl (\acc (Tuple indx val) -> fromMaybe [] (modifyAt indx (\x -> snoc x val) acc)) a updates 

            updates :: Array (Tuple Int BigInt)
            updates = foldl (\acc x -> snoc acc (Tuple (whereToUpdate x) ((newWorry x)`mod` (fromInt ohTheMisery)))) [] (fromMaybe [] $ a !! i)

            whereToUpdate :: BigInt -> Int
            whereToUpdate v = if (newWorry v) `mod` fromInt (fromMaybe 1 $ tests !! i) == fromInt 0 then fromMaybe 0 $ trs !! i else fromMaybe 0 $ fls !! i 

            newWorry :: BigInt -> BigInt
            newWorry v = (operationExecuter' v (fromMaybe ["old","*","1"] $ ops !! i)) -- v is old value i is current monkey

            ohTheMisery = foldl (\acc x -> acc * x ) 1 tests 



numberOfInspectsInRound' :: Array (Array BigInt) -> Array (Array String) -> Array Int -> Array Int -> Array Int -> Array BigInt
numberOfInspectsInRound' invs ops tests trs fls = ohgod invs 0 (replicate (length invs) (fromInt 0)) 
    where
      ohgod :: Array (Array BigInt) -> Int -> Array BigInt -> Array BigInt
      ohgod a i inspects 
            | i == (length a) = inspects
            | otherwise = ohgod newInvs (i+1) newInspects
        where 
            newInspects = fromMaybe [] $ modifyAt i (\x -> x + (fromInt (length $ fromMaybe [] $ a !! i))) inspects

            newInvs = fromMaybe [] $ updateAt i [] $ foldl (\acc (Tuple indx val) -> fromMaybe [] (modifyAt indx (\x -> snoc x val) acc)) a updates 

            updates :: Array (Tuple Int BigInt)
            updates = foldl (\acc x -> snoc acc (Tuple (whereToUpdate x) ((newWorry x)`mod` (fromInt ohTheMisery)))) [] (fromMaybe [] $ a !! i)

            whereToUpdate :: BigInt -> Int
            whereToUpdate v = if (newWorry v) `mod` fromInt (fromMaybe 1 $ tests !! i) == fromInt 0 then fromMaybe 0 $ trs !! i else fromMaybe 0 $ fls !! i 

            newWorry :: BigInt -> BigInt
            newWorry v = (operationExecuter' v (fromMaybe ["old","*","1"] $ ops !! i)) -- v is old value i is current monkey

            ohTheMisery = foldl (\acc x -> acc * x ) 1 tests 

            

allExecuteXRounds :: Int -> Array (Array Int) -> Array (Array String) -> Array Int -> Array Int -> Array Int -> List (Array (Array Int)) 
allExecuteXRounds noOfTimes invs ops tests trs fls = foldl (\acc _ -> List.snoc acc (executeRound (fromMaybe [] $ List.last acc) ops tests trs fls)) (invs : Nil) (replicate noOfTimes 0)

allExecuteXRounds' :: Int -> Array (Array BigInt) -> Array (Array String) -> Array Int -> Array Int -> Array Int -> List (Array (Array BigInt)) 
allExecuteXRounds' noOfTimes invs ops tests trs fls = foldl (\acc _ -> List.snoc acc (executeRound' (fromMaybe [] $ List.last acc) ops tests trs fls)) (invs : Nil) (replicate noOfTimes 0)

main :: Effect Unit
main = do 
    amogus <- input
    let eachMonkey = split (Pattern "\r\n\r\n") amogus
    let instructions = split (Pattern "\r\n") <$> eachMonkey
    let removeMonkeyNumber = (\x -> fromMaybe [] $ tail x) <$> instructions

    let initialMonkeysInventories = parseStartingItems <$> ((\x -> fromMaybe "" $ x !! 0) <$> removeMonkeyNumber)
    let operations = parseOperation <$> ((\x -> fromMaybe "" $ x !! 1) <$> removeMonkeyNumber)
    let tests = parseTest <$> ((\x -> fromMaybe "" $ x !! 2) <$> removeMonkeyNumber)
    let trueValues = parseIfTrue <$> ((\x -> fromMaybe "" $ x !! 3) <$> removeMonkeyNumber)
    let falseValues = parseIfFalse <$> ((\x -> fromMaybe "" $ x !! 4) <$> removeMonkeyNumber)

    logShow initialMonkeysInventories
    logShow operations
    logShow tests
    logShow trueValues
    logShow falseValues

    let rounds19 = allExecuteXRounds 19 initialMonkeysInventories operations tests trueValues falseValues
    let solution1 = foldl (\acc x -> acc * x) 1 $ takeEnd 2 $ sort (List.foldl (\acc x -> zipWith (+) acc x) (replicate (length initialMonkeysInventories) 0) $ (\x -> numberOfInspectsInRound x operations tests trueValues falseValues) <$> rounds19)

    let initialMonkeysInventories' = (\x -> fromInt <$> x) <$> initialMonkeysInventories  
    let rounds199 = allExecuteXRounds' 9999 initialMonkeysInventories' operations tests trueValues falseValues
    let solution2 = foldl (\acc x -> acc * x) (fromInt 1) $ takeEnd 2 $ sort (List.foldl (\acc x -> zipWith (+) acc x) (replicate (length initialMonkeysInventories) (fromInt 0)) $ (\x -> numberOfInspectsInRound' x operations tests trueValues falseValues) <$> rounds199)



    -- logShow rounds199


    logShow $ solution1
    logShow $ 10300

    logShow $ (List.foldl (\acc x -> zipWith (+) acc x) (replicate (length initialMonkeysInventories) (fromInt 0)) $ (\x -> numberOfInspectsInRound' x operations tests trueValues falseValues) <$> rounds199)
    logShow $ solution2
