module Day12 where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Relude.Unsafe as Unsafe

--import qualified Data.List as L
import qualified Data.Text     as T

import qualified Data.List.Extra as L

day12 :: IO ()
day12 = do
--  t <- readFileText "input/i12" --10 --36
--  t <- readFileText "input/in12" --19 --103
  t <- readFileText "input/input12" --3708
  putTextLn $ "day 10 " <> (show (run1 t))

type Paths = [Path]
type Path = [Text]

type CaveMap = Map Text [Text]
--type ReturnA = [(Text , [Text])]
--type Return = Int

run1 :: Text -> Int
run1 t = length $ buildFromStart $ Map.fromList $ bbb t

buildFromStart :: CaveMap -> Paths
buildFromStart caveMap = build caveMap [] "start"

--build :: Text -> Paths -> Paths -> CaveMap -> Paths
build :: CaveMap -> Path -> Text -> Paths
build caveMap current name
  | name == "start" && current /= [] = []
  | isLower name && (2 <= smallCaveLen || (1 == smallCaveLen && finalCondition current)) = []
  | name == "end" = [current <> ["end"]]
  | otherwise     = paths
    where
      smallCaveLen = smallCave current name
      paths = join $ ((build caveMap current') <$> names)
      names     = caveMap Map.! name
      current' = current <> [name]

isLower :: Text -> Bool
isLower t = t == T.toLower t


smallCave :: Path -> Text -> Int
smallCave current name = length (filter (\ e -> e == name) current)

finalCondition :: Path -> Bool
finalCondition current = 1 <= (length $ filter (\(k , v) -> isLower k && 2 <= v) (fff current))

fff :: Path -> [(Text, Int)]
fff current = (eee <$> (group $ sort current))

eee :: [Text] -> (Text, Int)
eee [] = error "ddd"
eee l@(e : _) = (e, length l)



bbb :: Text -> [(Text , [Text])]
bbb t = ccc <$> (L.groupOn (\ (k , _) -> k) $ sort $ aaa =<< (T.splitOn "-" <$> lines t))
--bbb t = ccc <$> (L.groupBy (\ (k1 , _) (k2 , _) -> k1 == k2) $ aaa =<< (T.splitOn "-" <$> lines t))

ccc :: [(Text , Text)] -> (Text, [Text])
ccc l@((k, _) : _) = (k , (\(_ , v) -> v) <$> l)
ccc []             = error "ccc"

aaa :: [Text] -> [(Text , Text)]
aaa [a1 , a2] = [(a1 , a2) , (a2 , a1)]
aaa  a        = error $ show a


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
