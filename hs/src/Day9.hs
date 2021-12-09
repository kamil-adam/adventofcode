module Day9 where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Relude.Unsafe as Unsafe

--import qualified Data.List as L
import qualified Data.Text     as T

day9 :: IO ()
day9 = do
--  content <- readFileText "input/i9"
  content <- readFileText "input/input9"
  putTextLn $ "day 9 " <> (show $ run content)

type Return = Int

run :: Text -> Return
run t = aaa $ (map readIntFromChar) <$> toString <$> lines t

aaa :: Matrix -> Return
aaa m = bbb m (yyy m , xxx m) (range0 $ yyy m) (range0 $ xxx m)

range0 :: Int -> [Int]
range0 n = [0 .. n]

yyy :: [[Int]] -> Int
yyy l = length l - 1

xxx :: [[Int]] -> Int
xxx l = (length $ l Unsafe.!! 0) - 1

bbb :: Matrix -> Point -> [Int] -> [Int] -> Int
bbb m n l1 l2 = sum $ (\ i -> (getCell m i) + 1) <$> ccc m n l1 l2

ccc :: Matrix -> Point -> [Int] -> [Int] -> [Point]
ccc m n l1 l2 = catMaybes $ ddd m n l1 l2

ddd :: Matrix -> Point -> [Int] -> [Int] -> [Maybe Point]
ddd m n l1 l2 = do
  i1 <- l1
  i2 <- l2
  pure $ checkCell m n (i1 , i2)

checkCell :: Matrix -> Point -> Point -> Maybe Point
--checkCell _ (i1, i2) (_, _) = Just (i1, i2)
checkCell m n i@(i1 , i2) = compareCells m i [maybePoint m n (i1-1 , i2-1) , maybePoint m n (i1-1 , i2) , maybePoint m n (i1-1 , i2+1) , maybePoint m n (i1 , i2-1) , maybePoint m n (i1 , i2+1) , maybePoint m n (i1+1 , i2-1) , maybePoint m n (i1+1 , i2) , maybePoint m n (i1+1 , i2+1)]

compareCells :: Matrix -> Point -> [Maybe Point] -> Maybe Point
compareCells m p l
  | all (compareCell m p) l = Just $ p
  | otherwise = Nothing

compareCell :: Matrix -> Point -> Maybe Point -> Bool
compareCell m p (Just p') = getCell m p < getCell m p'
compareCell _ _  Nothing  = True

maybePoint :: Matrix -> Point -> Point -> Maybe Point
maybePoint _ (n1, n2) i@(i1, i2)
  | 0 <= i1 && 0 <= i2 && i1 <= n1 && i2 <= n2 = Just i
  | otherwise = Nothing

getCell :: Matrix -> Point -> Int
getCell m (y , x) = (m Unsafe.!! y) Unsafe.!! x

type Matrix = [[Int]]

type Point = (Int, Int)

----

mull :: [Int] -> Int
mull [a1, a2, a3, a4] = a1 * 1000 + a2 * 100 + a3 * 10 + a4
mull a                = error $ show a

decode :: Map Int Text -> Text -> Int
decode m t
  | (T.length t == 2) = 1
  | (T.length t == 3) = 7
  | (T.length t == 4) = 4
  | (T.length t == 7) = 8
  | (T.length t == 6) && (contestText t (m Map.! 7)) && (contestText t (m Map.! 4)) = 9
  | (T.length t == 6) && (contestText t (m Map.! 7))  = 0
  | (T.length t == 6) = 6
  | (T.length t == 5) && (contestText t (m Map.! 1)) = 3
  | (T.length t == 5) && (contestText' t (m Map.! 4)) == 3 = 5
  | (T.length t == 5) && (contestText' t (m Map.! 4)) == 2 = 2
  | otherwise = error $ show m <> " " <> show t <> " " <> (show $ T.length t)

contestText :: Text -> Text -> Bool
contestText t2 t1 = Set.isSubsetOf (textToCharSet t1) (textToCharSet t2)

contestText' :: Text -> Text -> Int
contestText' t2 t1 = Set.size $ Set.intersection (textToCharSet t1) (textToCharSet t2)


textToCharSet :: Text -> Set Char
textToCharSet t = fromList $ toString t

findSimple :: [Text] -> Map Int Text
findSimple t = fromList [findPattern 1 2 t , findPattern 7 3 t , findPattern 4 4 t , findPattern 8 7 t]

findPattern :: Int -> Int -> [Text] -> (Int, Text)
findPattern i s l = (i , getMaybe $ find (\e -> T.length e == s) l)

getMaybe :: Maybe a -> a
getMaybe (Just a) = a
getMaybe Nothing  = error "e"


--ccc
--ccc 2 = 1
--ccc 3 = 7
--ccc 4 = 4
--ccc 7 = 8

--ddd

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

readIntFromChar :: Char -> Int
readIntFromChar c = readInt $ toText $ [c]

readInt :: Text -> Int
readInt = readUnsafe

readUnsafe :: Read a => Text -> a
readUnsafe t = (unsafe . readEither . toString) t where
  unsafe (Right a) = a
  unsafe (Left a)  = error $ a <> " " <> toText t

----
