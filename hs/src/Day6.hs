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
--  putTextLn $ "day 6 " <> (show $ run3 content)
  putTextLn $ "day 6 " <> (show $ sum3 $ run3 content)
--  putTextLn $ "day 6 " <> (show $ length $ run1 content)

type Element = (Int , Int)

type Result3 = [Element]

sum3 :: Result3 -> Int
sum3 r = sum $ ((\ (_, s) -> s) <$> r)

run3 :: Text -> Result3
run3 t = aaa3 $ lines t

aaa3 :: [Text] -> Result3
aaa3 [t] = bbb3 t
aaa3 t = error $ show t

bbb3 :: Text -> Result3
bbb3 t = nextDay3 0 256 $ frequency (readInt <$> T.splitOn "," t)

nextDay3 :: Int -> Int -> Result3 -> Result3
nextDay3 current expected values
  | current == expected = values
  | otherwise           = nextDay3 (current + 1) expected (l ++ [(8 , n)])  where
    (l , n) = foldr nextDayBody3 ([] , 0) values

nextDayBody3 :: Element -> (Result3, Int) -> (Result3, Int)
nextDayBody3 (0 , s) (l , n) = (([(6    , s)] <> l) , n + s)
nextDayBody3 (e , s) (l , n) = (([(e - 1, s)] <> l) , n)


----

--type Result3 = Map Int Int
--
--run2 :: Text -> Result3
--run2 t = aaa2 $ lines t
--
--aaa2 :: [Text] -> Result3
--aaa2 [t] = bbb2 t
--aaa2 t = error $ show t
--
--bbb2 :: Text -> Result3
--bbb2 t = nextDay2 0 80 $ frequency0 (readInt <$> T.splitOn "," t)
--
--nextDay2 :: Int -> Int -> Result3 -> Result3
--nextDay2 current expected values
--  | current == expected = values
--  | otherwise           = nextDay2 (current + 1) expected (Map.insert 8 n l) where
--    (l , n) = Map.foldr nextDayBody2 (Map.fromList [] , 0) values
--
--nextDayBody2 :: (Int, Int) -> (Result3 , Int) -> (Result3, Int)
--nextDayBody2 (0 , s) (l , n) = ((Map.insert 6 s l) , n + s)
--nextDayBody2 (e , s) (l , n) = ((Map.insert (e - 1) s l) , n)

----

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