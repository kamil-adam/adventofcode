--{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples, TypeFamilies #-}
--{-# LANGUAGE TemplateHaskellQuotes #-}
--{-# LANGUAGE Trustworthy #-}

module Day7 where

--import qualified          Relude.Unsafe as Unsafe

--import           Data.List.Split

import qualified Data.Map as Map

import qualified Data.List as L
import qualified Data.Text as T

day7 :: IO ()
day7 = do
--  content <- readFileText "input/i7"
  content <- readFileText "input/input7"
  putTextLn $ "day 7 " <> (show $ run content)

run :: Text -> (Int , Int)
run t = minForTuple $ (\i -> aaa i positions) <$> [(L.minimum positions) .. (L.maximum positions)]
--run t = (\i -> aaa i positions) <$> [(L.minimum positions) .. (L.maximum positions)]
  where positions = readInt <$> T.splitOn "," t

minForTuple :: [(Int , Int)] -> (Int , Int)
minForTuple l = L.foldl1 bbb l

bbb :: (Int , Int) -> (Int , Int) -> (Int , Int)
bbb (k1 , v1) (k2 , v2)
  | v1 <= v2  = (k1 , v1)
  | otherwise = (k2 , v2)

aaa :: Int -> [Int] -> (Int , Int)
aaa i l = (i , sum $ normalize i l)

normalize :: Int -> [Int] -> [Int]
normalize i l = (\ i' -> abs $ i' - i) <$> l

------

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

frequency0 :: (Ord a) => [a] -> Map a Int
frequency0 xs = (Map.fromListWith (+) [(x, 1) | x <- xs])


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