--{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples, TypeFamilies #-}
--{-# LANGUAGE TemplateHaskellQuotes #-}
--{-# LANGUAGE Trustworthy #-}

module Day4 where

--import           Relude.Unsafe

--import           Data.List.Split

import qualified Data.Text as T

day4 :: IO ()
day4 = do
  content <- readFileText "input/i4"
--  content <- readFileText "input/input4"
  putTextLn $ show $ run1 content

run1 :: Text -> (Maybe Int , Maybe Int)
run1 s = (aaa $ lines s)

aaa :: [Text] -> (Maybe Int , Maybe Int)
aaa (numbers : _ : a) = bbb (readInt <$> (T.splitOn "," numbers)) ((map (map (Right . readInt))) <$> (map words) <$> lines <$> toText <$>  (T.splitOn "\n\n" $ unlines a))
aaa t = error $ show t

bbb :: [Int] -> [Board] -> (Maybe Int , Maybe Int)
bbb numbers boards = (number , eee <$> (find checkBoard $ reverse $ checked))
  where (number , checked) = ccc numbers boards

eee :: Board -> Int
eee board = sum $ ggg <$> filter isRight (id =<< board)

fff :: Cell -> Int
fff (Left i) = i
fff (Right i) = error $ show i

ggg :: Cell -> Int
ggg (Right i) = i
ggg (Left i) = error $ show i

ccc :: [Int] -> [Board] -> (Maybe Int , [Board])
ccc [] boards = (Nothing , boards)
ccc (x : xs) boards =  ddd x xs (markCellInBoards x boards)

ddd :: Int -> [Int] -> [Board] -> (Maybe Int , [Board] , [Board])
ddd x xs boards =  if any id (checkBoards boards) then ((Just x) , boards) else ccc xs boards

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
readUnsafe = unsafe . readEither . toString where
  unsafe (Right a) = a
  unsafe (Left a)  = error a

