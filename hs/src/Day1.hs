module Day1 where

import           Data.List.Split

day1 :: IO ()
day1 = do
  content <- readFileText "input/day1"
  putTextLn $ show $ aaa content


aaa :: Text -> Int
aaa s = length $ filter increased $ divvy2 $ readInt <$> lines s

readInt :: Text -> Int
readInt = readUnsafe

divvy2 :: [Int] -> [[Int]]
divvy2 = divvy 2 1

increased :: [Int] -> Bool
increased (a : b : _) = a < b
increased l           = error $ show l

readUnsafe :: Read a => Text -> a
readUnsafe = unsafe . readEither . toString where
  unsafe (Right a) = a
  unsafe (Left a)  = error a

