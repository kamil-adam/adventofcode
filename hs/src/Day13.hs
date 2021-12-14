module Day13 where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Relude.Unsafe as Unsafe

--import qualified Data.List     as L
import qualified Data.Text     as T

import qualified Data.List.Extra as L

day13 :: IO ()
day13 = do
  t <- readFileText "input/i13" -- NNCB
--  t <- readFileText "input/i13" --17
--  t <- readFileText "input/in13" --19 --103
--  t <- readFileText "input/input13" -- SHPPPVOFPBFCHHBKBNCV
  putStrLn $ "day 10 \n" <>  (run1 "NNCB" t)
--  putStrLn $ "day 10 \n" <>  (run1 "SHPPPVOFPBFCHHBKBNCV" t)

type Return = Int

run1 :: Text -> Text -> String
run1 start t = step 10 (toString start) (buildMatchMap t)

step :: Int -> String -> MatchMap -> String
step 0 s _        = s
step i s matchMap = step (i - 1) s' matchMap
  where s' = nextState matchMap s

nextState :: MatchMap -> String -> String
nextState _                 [] = []
nextState _                [c] = [c]
nextState matchMap (c1: c2: s) = c1 : (match2chars matchMap c1 c2) : (nextState matchMap (c2:s))

buildMatchMap :: Text -> MatchMap
buildMatchMap t = buildMap <$> (buildMap $ buildMatchMap' <$> T.splitOn " -> " <$> lines t)

buildMap :: (Ord k , Ord v) => [(k , v)] -> Map k [v]
buildMap l = Map.fromList (aaa <$> (L.groupOn (\(k , _) -> k) $ sort l))

aaa :: [(k , v)] -> (k , [v])
aaa [] = error "aaa"
aaa l@((k , _) : _) = (k, (snd <$> l))

buildMatchMap' :: [Text] -> (Char , (Char , Char))
buildMatchMap' [t1 , t2] = (T.index t1 0 , (T.index t1 1 , T.index t2 0))
buildMatchMap' other     = error $ show other

type MatchMap = Map Char (Map Char [Char])

match2chars :: MatchMap -> Char -> Char -> Char
match2chars matchMap c1 c2 = ((matchMap Map.! c1) Map.! c2) Unsafe.!! 0

------

type Board = [Line]
type Line  = [Cell]
type Cell = Int

--------

check2 :: Char -> Int
check2 ')' = 1
check2 ']' = 2
check2 '}' = 3
check2 '>' = 4
check2  c  = error $ show c

middle :: [a] -> a
middle l = l Unsafe.!! (((length l) `div` 2))

-- 2441
-- )}>]}) = ((((((1 * 5 + 3) * 5 + 4) * 5) + 2) * 5) +  3) * 5 + 1

------

--type Return = [Maybe Int]
--type Return = Int

------

rmdups :: (Ord a) => [a] -> [a]
rmdups = map Unsafe.head . group . sort

findCells :: Matrix -> Point -> Point -> [Point]
findCells m n i = findCells2 m n i $ getCell m i

findCells2 :: Matrix -> Point -> Point -> Int -> [Point]
findCells2 m n i@(i1 , i2) v
  | 9 == v = []
--  | otherwise = i : (catMaybes $ [maybePoint m n (i1-1 , i2) , maybePoint m n (i1 , i2-1) , maybePoint m n (i1 , i2+1) , maybePoint m n (i1+1 , i2)])
  | otherwise = i : (findCells m n =<< (filter (\i' -> checkCell9 v (getCell m i')) $ catMaybes $ [maybePoint m n (i1-1 , i2) , maybePoint m n (i1 , i2-1) , maybePoint m n (i1 , i2+1) , maybePoint m n (i1+1 , i2)]))

checkCell9 :: Int -> Int -> Bool
checkCell9 v v' = v < v' && v' < 9

minimums :: Matrix -> Point -> [Int] -> [Int] -> [Point]
minimums m n l1 l2 = catMaybes $ ddd m n l1 l2

ddd :: Matrix -> Point -> [Int] -> [Int] -> [Maybe Point]
ddd m n l1 l2 = do
  i1 <- l1
  i2 <- l2
  pure $ checkCell m n (i1 , i2)

checkCell :: Matrix -> Point -> Point -> Maybe Point
checkCell m n i@(i1 , i2) = compareCells m i [maybePoint m n (i1-1 , i2) , maybePoint m n (i1 , i2-1) , maybePoint m n (i1 , i2+1) , maybePoint m n (i1+1 , i2)]

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

findSimple :: [Text] -> Map Int Text
findSimple t = fromList [findPattern 1 2 t , findPattern 7 3 t , findPattern 4 4 t , findPattern 8 7 t]

findPattern :: Int -> Int -> [Text] -> (Int, Text)
findPattern i s l = (i , getMaybe $ find (\e -> T.length e == s) l)

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
