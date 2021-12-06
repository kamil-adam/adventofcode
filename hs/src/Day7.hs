--{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples, TypeFamilies #-}
--{-# LANGUAGE TemplateHaskellQuotes #-}
--{-# LANGUAGE Trustworthy #-}

module Day7 where

--import qualified          Relude.Unsafe as Unsafe

--import           Data.List.Split

import qualified Data.Map as Map

import qualified Data.Text as T

day7 :: IO ()
day7 = do
--  content <- readFileText "input/i7"
  content <- readFileText "input/input7"
  putTextLn $ "day 7 " <> (show $ run3 content)
--  putTextLn $ "day 7 " <> (show $ sum3 $ run3 content)
--  putTextLn $ "day 7 " <> (show $ length $ run1 content)


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