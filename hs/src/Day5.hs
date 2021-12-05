--{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples, TypeFamilies #-}
--{-# LANGUAGE TemplateHaskellQuotes #-}
--{-# LANGUAGE Trustworthy #-}

module Day5 where

--import qualified          Relude.Unsafe as Unsafe

--import           Data.List.Split

import qualified Data.Map as Map

import qualified Data.Text as T

day5 :: IO ()
day5 = do
--  content <- readFileText "input/i5"
  content <- readFileText "input/input5"
  putTextLn $ "day 5 " <> (show $ run1 content)

run1 :: Text -> Int
--run1 t = filter (\ ((x1 , y1) , (x2 , y2)) -> x1 == x2 || y1 == y2) b
--  where b = aaa <$> lines t
run1 t = length $ filter (\ (_, i) -> 2 <= i) $ frePoints t

frePoints :: Text -> [(Point, Int)]
frePoints  t = frequency $ allPoints t



allPoints :: Text -> [Point]
allPoints t = points =<< ((aaa <$> lines t))

aaa :: Text -> Line
--aaa t = intList <$> splitOnC <$> T.splitOn " -> " t
aaa t = bbb $ intList <$> splitOnC <$> splitOnA t

points :: Line -> [Point]
points ((x1 , y1) , (x2 , y2))
  | x1 == x2 = (\ y -> (x1 , y)) <$> (range y1 y2)
  | y1 == y2 = (\ x -> (x , y1)) <$> (range x1 x2)
  | otherwise = []
--points l = error $ "points " <> show l

range :: Int -> Int -> [Int]
range z1 z2
  | z1 <= z2  = [z1 .. z2]
  | otherwise = [z2 .. z1]

splitOnA :: Text -> [Text]
splitOnA t = T.splitOn " -> " t

splitOnC :: Text -> [Text]
splitOnC t = T.splitOn "," t

intList :: [Text] -> [Int]
intList l = readInt <$> l

--ccc <$> chunksOf 4 $



bbb :: [[Int]] -> Line
bbb [[x1 , y1], [x2 , y2]] = ((x1 , y1) , (x2 , y2))
--bbb [[x1 , y1 , x2 , y2]] = ((x1 , y1) , (x2 , y2))
bbb t = error $ "[[x1 , y1], [x2 , y2]] " <> show t

ccc :: [Int] -> Line
ccc [x1 , y1 , x2 , y2] = ((x1 , y1) , (x2 , y2))
ccc t = error $ "[x1 , y1, x2 , y2] " <> show t

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