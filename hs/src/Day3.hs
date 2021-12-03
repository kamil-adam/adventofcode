module Day2 where

day3 :: IO ()
day3 = do
  content <- readFileText "input/input3"
  putTextLn $ show $ aaa content

run :: Text -> [Bool]
run s = ccc <$> transpose $ aaa <$> lines s

count :: [Text] -> Int
count l = length l

aaa :: Text -> [Bool]
aaa l = bbb <$> toString l

bbb :: Char -> Bool
bbb '0' = False
bbb '1' = True
bbb  c  = error $ show c

ccc :: [Bool] -> Bool
ccc l = 1/2 <= (length $ filter id l)/(length l)

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

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

newtype ZipList a = ZipList { getZipList :: [a] }

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)


------

readInt :: Text -> Int
readInt = readUnsafe

readUnsafe :: Read a => Text -> a
readUnsafe = unsafe . readEither . toString where
  unsafe (Right a) = a
  unsafe (Left a)  = error a

