module Day3 where

import           Relude.Unsafe

day3 :: IO ()
day3 = do
  content <- readFileText "input/input3"
  putTextLn $ show $ run2 content

run :: Text -> [Bool]
run s = ccc <$> transpose2 (aaa <$> lines s)

run2 :: Text -> [Bool]
run2 s = aaa3 $ (aaa <$> lines s)

-- [False,True,False,False,True,False,False,False,True,False,False,False]
--010010001000 = 1160


aaa3 :: [[Bool]] -> [Bool]
aaa3 l
  | 0 < length l = index2 "aaa3" (aaa4 0 l) 0
  | otherwise = error "empty list"

aaa4 :: Int -> [[Bool]] -> [[Bool]]
aaa4 i l
  | 1 == (length l) = l
  | i < (length $ index2 "aaa4" l 0) = aaa4 (i + 1) (filterList i l)
  | otherwise = l

filterList :: Int -> [[Bool]] -> [[Bool]]
filterList i l
  | 1 == (length l) = l
  | i < (length $ index2 "filterList" l 0) = filter (\al -> (index2 "filterList" al i) == (notCCC $ bbb2 i <$> l)) l
  | otherwise = error $ show i <> show l



bbb2 :: Int -> [Bool] -> Bool
bbb2 i l = l !! i


index2:: (Show a) => Text -> [a] -> Int -> a
index2 s l i
  | 0 < length l = l !! i
  | otherwise = error $ show s <> " " <> show i <> " " <> show l



--111001011100 = 3676
--000110100011 = 419
--1540244

aaa :: Text -> [Bool]
aaa l = bbb <$> toString l

bbb :: Char -> Bool
bbb '0' = False
bbb '1' = True
bbb  c  = error $ show c

notCCC :: [Bool] -> Bool
notCCC = not . ccc

ccc :: [Bool] -> Bool
ccc l = (2 * length (filter id l)) <= length l

commend :: [Text] -> (Int , Int)
commend ["forward" , a] = (readInt a , 0)
commend ["up"      , a] = (0 , negate $ readInt a)
commend ["down"    , a] = (0 , readInt a)
commend c               = error $ show c

sumList :: [(Int , Int)] -> (Int , Int , Int)
--sumList = fold sum2
--sumList = foldr sum2 (0 , 0) -- (forward , depth)
sumList = foldl' sum3 (0 , 0 , 0) -- (forward , depth , aim)

sum3 :: (Int , Int , Int) -> (Int , Int) -> (Int , Int , Int)
sum3  (b1 , b2 , b3) (a1 , a2) = ( b1 + a1 , b2 + (b3 * a1) ,  b3 + a2)

sum2 :: (Int , Int) -> (Int , Int) -> (Int , Int)
sum2 (a , b) (c , d) = (a + c , b + d)

productPair3 :: (Int , Int , Int) -> Int
productPair3 (a , b , _) = a * b

productPair :: (Int , Int) -> Int
productPair (a , b) = a * b

----

transpose2 :: [[a]] -> [[a]]
transpose2 = getZipList . traverse ZipList

--newtype ZipList a = ZipList { getZipList :: [a] }
--
--instance Applicative ZipList where
--    pure x = ZipList (repeat x)
--    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)


------

readInt :: Text -> Int
readInt = readUnsafe

readUnsafe :: Read a => Text -> a
readUnsafe = unsafe . readEither . toString where
  unsafe (Right a) = a
  unsafe (Left a)  = error a

