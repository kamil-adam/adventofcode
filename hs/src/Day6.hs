--{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples, TypeFamilies #-}
--{-# LANGUAGE TemplateHaskellQuotes #-}
--{-# LANGUAGE Trustworthy #-}

module Day6 where

--import qualified          Relude.Unsafe as Unsafe

--import           Data.List.Split

import qualified Data.Map as Map

import qualified Data.Text as T

day6 :: IO ()
day6 = do
--  content <- readFileText "input/i6"
  content <- readFileText "input/input6"
--  putTextLn $ "day 6 " <> (show $ run1 content)
  putTextLn $ "day 6 " <> (show $ length $ run1 content)

run1 :: Text ->  [Int]
run1 t = aaa $ lines t

aaa :: [Text] ->  [Int]
aaa [t] = bbb t
aaa t = error $ show t

bbb :: Text ->  [Int]
bbb t = nextDay 0 80 (readInt <$> T.splitOn "," t)

nextDay :: Int -> Int -> [Int] -> [Int]
nextDay current expected values
  | current == expected = values
  | otherwise           = nextDay (current + 1) expected ((reverse l) ++ (newFish n ))  where
    (l , n) = foldr  nextDayBody ([] , 0) values

nextDayBody :: Int -> ([Int], Int) -> ([Int], Int)
nextDayBody 0 (l , n) = (l <> [6]       , n + 1)
nextDayBody e (l , n) = (l <> [(e - 1)] , n)

newFish :: Int -> [Int]
newFish n = (\ _ -> 8) <$> [1 .. n]

range :: Int -> Int -> [Int]
range z1 z2
  | z1 <= z2  = [z1 .. z2]
  | otherwise = reverse [z2 .. z1]

splitOnA :: Text -> [Text]
splitOnA t = T.splitOn " -> " t

splitOnC :: Text -> [Text]
splitOnC t = T.splitOn "," t

intList :: [Text] -> [Int]
intList l = readInt <$> l

--ccc <$> chunksOf 4 $


type Line = (Point , Point)
type Point = (Int , Int)










markCellInBoards :: Int -> [Board] -> [Board]
markCellInBoards i = map (markCellInBoard i)

markCellInBoard :: Int -> Board -> Board
markCellInBoard i = map (markCellInLine i)

markCellInLine :: Int -> [Cell] -> [Cell]
markCellInLine i = map (markCell i)

markCell :: Int -> Cell -> Cell
markCell i c = do
  c' <- c
  if i == c' then (Left c') else c


checkBoards :: [Board] -> [Bool]
checkBoards = map checkBoard

checkBoard :: Board -> Bool
checkBoard board = (checkBoard' board) || (checkBoard' $ transpose2 board)

checkBoard' :: Board -> Bool
checkBoard' board = any (\line -> all  (\ cell -> isLeft cell) line) board

type Board = [[Cell]]
type Cell = Either Int Int

------

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

------

transpose2 :: [[a]] -> [[a]]
transpose2 = getZipList . traverse ZipList

------

readInt :: Text -> Int
readInt = readUnsafe

readUnsafe :: Read a => Text -> a
readUnsafe t = (unsafe . readEither . toString) t where
  unsafe (Right a) = a
  unsafe (Left a)  = error $ a <> " " <> toText t

----