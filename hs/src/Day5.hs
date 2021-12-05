--{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples, TypeFamilies #-}
--{-# LANGUAGE TemplateHaskellQuotes #-}
--{-# LANGUAGE Trustworthy #-}

module Day5 where

--import qualified          Relude.Unsafe as Unsafe

--import           Data.List.Split

import qualified Data.Text as T

day5 :: IO ()
day5 = do
  content <- readFileText "input/i5"
--  content <- readFileText "input/input5"
  putTextLn $ show $ run1 content

run1 :: Text -> [Line]
--run1 t = filter (\ ((x1 , y1) , (x2 , y2)) -> x1 == x2 || y1 == y2) b
--  where b = aaa <$> lines t

run1 t = l
where l= aaa <$> lines t

aaa :: Text -> Line
--aaa t = intList <$> splitOnC <$> T.splitOn " -> " t
aaa t = bbb $ intList <$> splitOnC <$> splitOnA t

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

transpose2 :: [[a]] -> [[a]]
transpose2 = getZipList . traverse ZipList

------

readInt :: Text -> Int
readInt = readUnsafe

readUnsafe :: Read a => Text -> a
readUnsafe t = (unsafe . readEither . toString) t where
  unsafe (Right a) = a
  unsafe (Left a)  = error $ a <> " " <> toText t

