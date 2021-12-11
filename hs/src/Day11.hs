module Day11 where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Relude.Unsafe as Unsafe

--import qualified Data.List as L
import qualified Data.Text     as T

day11 :: IO ()
day11 = do
--  content <- readFileText "input/i10"
--  content <- readFileText "input/in10"
  content <- readFileText "input/input10"
  putTextLn $ "day 10 " <> (show $ run2 content)

--type Return2 = [Maybe Int]
--type Return2 = [Int]
type Return2 = Int

run2 :: Text -> Return2
run2 t = middle $ sort $ catMaybes $ start2 [] <$> toString <$> lines t
--run2 t = sort $ catMaybes $ start2 [] <$> toString <$> lines t
--run2 t = start2 [] <$> toString <$> lines t

start2 :: String -> String -> Maybe Int
--start2 _ []        = Nothing
start2 st   []     = Just $ checkState 0 st
--start st [c]      = compute st c []
start2 st (c : s)  = startWithChar2 st c s

startWithChar2 :: String -> Char -> String -> Maybe Int
startWithChar2 st '(' s = expect2 st ')' s
startWithChar2 st '[' s = expect2 st ']' s
startWithChar2 st '{' s = expect2 st '}' s
startWithChar2 st '<' s = expect2 st '>' s
startWithChar2 st  c  s = compute2 st c s

expect2 :: String -> Char -> String -> Maybe Int
--expect st c' []  = compute st c' []
expect2 st c' [] = Just $ checkState 0 (c' : st)
expect2 st c' (c : s)
  | c' == c   = start2 st s
  | otherwise = startWithChar2 (c' : st) c s

compute2 :: String -> Char -> String -> Maybe Int
compute2 (c' : st) c s
  | c' == c   = start2 st s
--  | otherwise = Just $ check2 c
  | otherwise = Nothing
compute2 st c  s  = error $ show c <> " " <> show s <> " " <> show st

checkState :: Int -> String -> Int
checkState st      [] = st
checkState st (c : s) = checkState ((st * 5) + (check2 c)) s
--checkState _      s = error $ show s

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
type Return = Int


run1 :: Text -> Return
run1 t = sum $ catMaybes $ start [] <$> toString <$> lines t

start :: String -> String -> Maybe Int
start _ []        = Nothing
--start st [c]      = compute st c []
start st (c : s)  = startWithChar st c s

startWithChar :: String -> Char -> String -> Maybe Int
startWithChar st '(' s = expect st ')' s
startWithChar st '[' s = expect st ']' s
startWithChar st '{' s = expect st '}' s
startWithChar st '<' s = expect st '>' s
startWithChar st  c  s = compute st c s

expect :: String -> Char -> String -> Maybe Int
--expect st c' []  = compute st c' []
expect _ _ [] = Nothing
expect st c' (c : s)
  | c' == c   = start st s
  | otherwise = startWithChar (c' : st) c s

compute :: String -> Char -> String -> Maybe Int
compute (c' : st) c s
  | c' == c   = start st s
  | otherwise = Just $ check c
compute st c  s  = error $ show c <> " " <> show s <> " " <> show st

check :: Char -> Int
check ')' = 3
check ']' = 57
check '}' = 1197
check '>' = 25137
check  c  = error $ show c

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
