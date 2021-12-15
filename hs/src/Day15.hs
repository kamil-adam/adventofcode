module Day15 where

import qualified Data.Map        as Map
import qualified Data.Set        as Set

import qualified Relude.Unsafe   as Unsafe

import qualified Data.List     as L
import qualified Data.Text       as T

import qualified Data.List.Extra as L

day15:: IO ()
day15 = do
--  t <- readFileText "input/i15"
--  t <- readFileText "input/i15" --17
--  t <- readFileText "input/in15" --19 --103
  t <- readFileText "input/input15"
  putTextLn $ "day 15 \n" <> (show $ (run11 t))

type Return = Int
--type Return = [Int]
--type Return = Board

--type Return1 = [[(Point, Int)]]
--type Return1 = [(Point, [Int])]
type Return1 = [(Point, Int)]

type Result1 = [(Point , Int)]

--run11:: Text -> Return1
--run11 :: Text -> [(Point, Int)]
run11 :: Text -> Int
--run11 t = L.minimum $ start1 $ map readIntFromChar <$> toString <$> lines t
--run11 t =  bbb L.minimum <$> aaa <$> (sortGroupOn (fst) $ start1 $ map readIntFromChar <$> toString <$> lines t)
--run11 t =  bbb L.minimum <$> aaa <$> (sortGroupOn (fst) $ start1end $ map readIntFromChar <$> toString <$> lines t)
--run11 t = bbb sum <$> aaa <$> sortGroupOn (fst) $ (startLine <> endLine)
run11 t =  L.minimum (snd <$> (bbb sum <$> grouped))
  where
    grouped   = aaa <$> (sortGroupOn (fst) $ (line))
    line      = startLine <> endLine
    startLine = bbb L.minimum <$> aaa <$> (sortGroupOn (fst) $ start1 b)
    endLine   = bbb L.minimum <$> aaa <$> (sortGroupOn (fst) $ start1end b)
    b = map readIntFromChar <$> toString <$> lines t

--ccc :: [(Point, Int)]-> (Point, [Int])
--ccc = aaa


start1end :: Board -> Result1
start1end b = step1end 0 pe end b where
  end = (length b) - 1
  pe  = ((length b) - 1, (length (Unsafe.head b)) - 1)

step1end :: Int -> Point -> Int -> Board -> Result1
step1end s p@(i1 , i2) end b
  | i1 + i2 <= end   = [(p , s)]
  | 0 < i1 && 0 < i2 = (step1end s' (i1 - 1 , i2) end b) <> (step1end s' (i1 , i2 - 1) end b)
  | 0 < i1           = (step1end s' (i1 - 1 , i2) end b)
  | 0 < i2           = (step1end s' (i1 , i2 - 1) end b)
  | otherwise        = [(p , s)]
    where
      s' = s + v
      v = valuePoint b p

start1 :: Board -> Result1
start1 b = step1 0 (0, 0) end pe b where
  pe  = ((length b) - 1, (length (Unsafe.head b)) - 1)
  end = (length b) - 1



step1 :: Int -> Point -> Int -> Point -> Board -> Result1
step1 s p@(i1 , i2) end pe@(e1 , e2) b
  | end <= i1 + i2      = [(p , s)]
  | i1 < e1 && i2 < e2 = (step1 s' (i1 + 1 , i2) end pe b) <> (step1 s' (i1 , i2 + 1) end pe b)
  | i1 < e1            = (step1 s' (i1 + 1 , i2) end pe b)
  | i2 < e2            = (step1 s' (i1 , i2 + 1) end pe b)
  | otherwise          = [(p , s)]
    where
      s' = s + v
      v = valuePoint b p

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

bbb :: ([v] -> v) -> (k, [v]) -> (k, v)
bbb f (k, vs) = (k , f vs)

aaa :: [(k, v)] -> (k, [v])
aaa l@((k, _): _) = (k , snd <$> l)
aaa [] = error "aaa"


valuePoint :: Board -> Point -> Int
valuePoint b (i1 , i2) = (b Unsafe.!! i1) Unsafe.!! i2

sortGroupOn :: (Ord a, Eq b) => (a -> b) -> [a] -> [[a]]
sortGroupOn f = L.groupOn f. sort

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
