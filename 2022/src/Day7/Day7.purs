module Day7
  ( main
  )
  where

import Prelude

import Data.Array (filter, foldl, tail)
import Data.Foldable (find, sum)
import Data.Int (fromString)
import Data.List (List(..), last, sort)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tree (Tree, appendChild, mkTree, scanTree, showTree)
import Data.Tree.Zipper (Loc, children, down, findDownWhere, flattenLocDepthFirst, fromTree, modifyNode, modifyValue, next, parents, root, setValue, toTree, up, value)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

input :: Effect String
input = readTextFile UTF8 "src/Day7/input.txt"

type NodeValue = { name:: String, value:: Int, visited:: Boolean, parentName:: String }

data Command = Up | Down String | File Int | Directory String
instance Show Command where
  show a = case a of 
    Up -> "Up"
    Down b -> "Down " <> b
    File b -> "File " <> show b
    Directory b -> "Directory " <> b


parseCommand :: String -> Maybe Command  
parseCommand s = case split (Pattern " ") s of 
    ["$", "cd", ".."] -> Just Up
    ["$", "cd", dir] -> Just $ Down dir
    ["dir", dir] -> Just $ Directory dir
    [value, _] -> Just $ File $ fromMaybe 0 $ fromString value
    _ -> Nothing

emptyTree :: Tree NodeValue
emptyTree = mkTree { name: "/", value: 0, visited: false, parentName: "" } (Nil)

executeCommand :: Maybe (Loc NodeValue) -> Maybe Command -> Maybe (Loc NodeValue) 
executeCommand loc c = case loc of 
    Just l ->  case c of 
        Just Up -> Just $ fromMaybe l $ up $ modifyValue (\a -> {name: a.name, value: a.value, visited: true, parentName: a.parentName}) l 
        Just (Down v) -> findDownWhere (\a -> a.name == v) $ modifyValue (\a -> {name: a.name, value: a.value, visited: true, parentName: a.parentName}) l
        Just (File v) -> Just $ modifyValue (\a -> {name: a.name, value: if not a.visited then a.value + v else a.value, visited: a.visited, parentName: a.parentName}) l
        Just (Directory v) -> Just $ modifyNode (\a -> appendChild (mkTree {name: v, value: 0, visited: false, parentName: (value l).name} (Nil)) a) l
        Nothing -> Nothing
    Nothing -> Nothing

getDirectorySize :: Loc Int -> Loc Int 
getDirectorySize t = if List.null $ children t then  
    t
    else do
      let childrenSizes = (fromTree <$> children t) <#> getDirectorySize
      let totalSize = sum (childrenSizes <#> value) + value t
      setValue totalSize t

flattenValues :: Tree NodeValue -> Tree Int
flattenValues t = scanTree (\a _ -> a.value) 0 t

walkTree :: forall a. (Loc a -> Loc a) -> Boolean -> Loc a -> Loc a
walkTree f skip l = case (List.null $ children l) || skip of
    true -> case next l of
        Just k -> walkTree f false k
        Nothing -> case List.null $ parents l of 
            true -> l 
            false -> walkTree f true $ fromMaybe l $ up l
    false  -> walkTree f false $ fromMaybe l $ down $ f l 

main :: Effect Unit
main = do 
    input' <- input
    let splitted = split (Pattern "\r\n") input' -- split the input
    let filterLss = filter (_ /= "$ ls") splitted -- filter all of the ls commands 
    let commands = fromMaybe [] $ tail $ parseCommand <$> filterLss -- parse the commands 
    let tree = toTree $ root $ fromMaybe (fromTree emptyTree) $ foldl (\acc n -> executeCommand acc n) (Just $ fromTree emptyTree) commands -- execute the commands to make a tree
    let flattenedTree = flattenValues tree -- flatten the values of the tree so you have a tree of Int values instead of NodeValues

    -- add the folder's children sizes to the folder 
    -- (so far it only had the size of the files inside that folder without the subfolders' sizes)
    let finalLoc = walkTree (\a -> getDirectorySize a) false $ fromTree flattenedTree 

    -- just sum all the folders that have a size less than 100k
    let solution1 = sum $ (\a -> if (value a) <= 100000 then value a else 0) <$> flattenLocDepthFirst finalLoc
    
    -- flatten the tree into a list and sort it 
    let sortedDirectoriesSizesList = sort $ (\a -> value a) <$> flattenLocDepthFirst finalLoc
    -- the size used is the root folder
    let sizeUsed = fromMaybe 0 $ last sortedDirectoriesSizesList
    -- find the first element in the sorted list that matches condition
    let solution2 = fromMaybe 0 $ find (\a -> 70000000 - sizeUsed + a > 30000000) sortedDirectoriesSizesList

    -- This took me too long, like 6-8 hours
    logShow solution1
    logShow solution2

    -- If you want to see the trees uncomment these lines or just check folderTrees.txt 
    -- log $ showTree tree
    -- log $ showTree $ toTree finalLoc

    -- I also want to mention that my solution is not the best.
    -- I could've parsed the input directly into a list of folders instead of working with trees
    -- But I wanted to work with trees lol
