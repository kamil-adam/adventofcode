module Day2 where

import           Data.List.Split

day2 :: IO ()
day2 = do
  content <- readFileText "input/day1"
  putTextLn $ show $ bbb content

bbb :: Text -> Int
bbb s = length $ filter increased $ divvy2 $ sum3 <$> divvy3 (readInt <$> lines s)

aaa :: Text -> Int
aaa s = length $ filter increased $ divvy2 $ readInt <$> lines s

readInt :: Text -> Int
readInt = readUnsafe

divvy3 :: [Int] -> [[Int]]
divvy3 = divvy 3 1

sum3 :: [Int] -> Int
sum3 (a : b : c : _) = a + b + c
sum3 l               = error $ show l

divvy2 :: [Int] -> [[Int]]
divvy2 = divvy 2 1

increased :: [Int] -> Bool
increased (a : b : _) = a < b
increased l           = error $ show l

readUnsafe :: Read a => Text -> a
readUnsafe = unsafe . readEither . toString where
  unsafe (Right a) = a
  unsafe (Left a)  = error a
