module Day2 where

import           Data.List.Split

day2 :: IO ()
day2 = do
  content <- readFileText "input/day1"
  putTextLn $ show $ aaa content

aaa :: Text -> Int
aaa s = productPair $ sumList $ commend <$> words <$> lines s

commend :: [Text] -> (Int , Int)
commend ("farward" : a : _) = (readInt a , 0)
commend ("up" : a : _)      = (0 , readInt a)
commend ("down" : a : _)    = (0 , negate (readInt a))

sumList :: [(Int , Int)] -> (Int , Int)
sumList = fold sum2

sum2 :: (Int , Int) -> (Int , Int) -> (Int , Int)
sum (a , b) (c , d) = (a + c , b + d)

productPair :: (Int , Int) -> Int
productPair (a , b) = a * b

readInt :: Text -> Int
readInt = readUnsafe

readUnsafe :: Read a => Text -> a
readUnsafe = unsafe . readEither . toString where
  unsafe (Right a) = a
  unsafe (Left a)  = error a
