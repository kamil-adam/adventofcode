module Day15 where

import qualified Data.Map        as Map
import qualified Data.Set        as Set

import qualified Relude.Unsafe   as Unsafe

import qualified Data.List     as L
import qualified Data.Text       as T

--import qualified Data.List.Extra as L

day15:: IO ()
day15 = do
  t <- readFileText "input/i15" -- NNCB
--  t <- readFileText "input/i15" --17
--  t <- readFileText "input/in15" --19 --103
--  t <- readFileText "input/input15" -- SHPPPVOFPBFCHHBKBNCV
  putTextLn $ "day 15 \n" <> (show $ (run1 t))

type Return = Int
--type Return = [Int]
--type Return = Board

run1 :: Text -> Return
run1 t = L.minimum $ start $ map readIntFromChar <$> toString <$> lines t

start :: Board -> [Int]
start b = step 0 (0, 0) end b where end = ((length b) - 1, (length (Unsafe.head b)) - 1)

step :: Int -> Point -> Point -> Board -> [Int]
step s p@(i1 , i2) pe@(e1 , e2) b
  | i1 < e1 && i2 < e2 = (step s' (i1 + 1 , i2) pe b) <> (step s' (i1 , i2 + 1) pe b)
  | i1 < e1            = (step s' (i1 + 1 , i2) pe b)
  | i2 < e2            = (step s' (i1 , i2 + 1) pe b)
  | otherwise          = [s]
    where
      s' = s + v
      v = valuePoint b p

valuePoint :: Board -> Point -> Int
valuePoint b (i1 , i2) = (b Unsafe.!! i1) Unsafe.!! i2




sortGroupAndCount :: (Ord a) => [a] -> [(a , Int)]
sortGroupAndCount l = countDuplicates <$> sortAndGroup l

countDuplicates :: [a] -> (a , Int)
countDuplicates      [] = error "countL"
countDuplicates l@(h:_) = (h, length l)

sortAndGroup :: (Ord a) => [a] -> [[a]]
sortAndGroup = group . sort

------

type Board = [Line]
type Line  = [Cell]
type Cell = Int

--------

middle :: [a] -> a
middle l = l Unsafe.!! (((length l) `div` 2))

------

rmdups :: (Ord a) => [a] -> [a]
rmdups = map Unsafe.head . group . sort

type Matrix = [[Int]]
type Unvisited  = [[Bool]]

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


getMaybe :: Maybe a -> a
getMaybe (Just a) = a
getMaybe Nothing  = error "e"


--bbb
--bbb 2 = 1
--bbb 3 = 7
--bbb 4 = 4
--bbb 7 = 8

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
