module Day2 where

day2 :: IO ()
day2 = do
  content <- readFileText "input/input2"
  putTextLn $ show $ aaa content

aaa :: Text -> Int
aaa s = productPair3 $ sumList $ commend . words <$> lines s

--commandList :: [Text] -> (Int , Int)
--commandList l = commend !!

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

readInt :: Text -> Int
readInt = readUnsafe

readUnsafe :: Read a => Text -> a
readUnsafe = unsafe . readEither . toString where
  unsafe (Right a) = a
  unsafe (Left a)  = error a
