module Day11 where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Relude.Unsafe as Unsafe

--import qualified Data.List as L
import qualified Data.Text     as T

day11 :: IO ()
day11 = do
--  content <- readFileText "input/i11"
--  content <- readFileText "input/in11"
  content <- readFileText "input/input11"
  putTextLn $ "day 10 " <> (show $ run2 content)

type Return = (Int, Board)

run2 :: Text -> Int
run2 t = step2 0 (0 , (map readIntFromChar <$> toString <$> lines t))

step2 :: Int -> BoardState -> Int
step2 i (r , b)
  | checkAllCell b = i
  | otherwise      = step2 (i + 1) $ checkBoard (r , b')
      where b' = add1ForBoard b

checkAllCell :: Board -> Bool
--checkAllCell b = b == (replicate 10 $ replicate 10 (0::Int))
checkAllCell b = 0 == (sum $ sum <$> b)

run1 :: Text -> Return
run1 t =  step 100 (0 , (map readIntFromChar <$> toString <$> lines t))

step :: Int -> BoardState -> BoardState
step 0 s = s
step i (r , b) = step (i - 1) $ checkBoard (r , b')
  where b' = add1ForBoard b

checkBoard  :: BoardState -> BoardState
checkBoard (r , b) = flash l (r , b)
  where
    l = catMaybes $ join( (\i1 -> ((\i2 -> checkPoint b (i1 , i2)) <$> [0 .. l2])) <$> [0 .. l1])
    l1 = (length b) - 1
    l2 = (length $ Unsafe.head b) - 1

flash :: [Point] -> BoardState -> BoardState
flash [] s = s
flash l (r , b) = checkBoard (r' , (incPoints l b'))
  where
    r' = r + length l
    b' = zeroPoints l b

incPoints :: [Point] -> Board -> Board
incPoints      [] b = b
incPoints (p : l) b = incPoints l $ incNHPoint p b

incNHPoint :: Point -> Board -> Board
incNHPoint p b = incNH (catMaybes (checkPointExists b <$> (nh p))) b

incNH :: [Point] -> Board -> Board
incNH [] b = b
incNH (p : l) b = incNH l $ incPointInNH p b

incPointInNH :: Point -> Board -> Board
incPointInNH (i1 , i2) b
  | value == 0 = b
  | otherwise  = (replaceInList i1 (replaceInList i2 (value + 1) (b Unsafe.!! i1)) b)
    where value = (b Unsafe.!! i1) Unsafe.!! i2

nh :: Point -> [Point]
nh (i1 , i2) =
       [(i1 - 1 , i2 - 1) , (i1 - 1 , i2) , (i1  - 1, i2 + 1)
       , (i1     , i2 - 1) ,                 (i1     , i2 + 1)
       , (i1 + 1 , i2 - 1) , (i1 + 1 , i2) , (i1  + 1, i2 + 1)
       ]

checkPointExists :: Board -> Point -> Maybe Point
checkPointExists b p@(i1 , i2)
  | 0 <= i1 && i1 < (length b) && 0 <= i2 && i2 < (length (Unsafe.head b)) = Just p
  | otherwise = Nothing

zeroPoints :: [Point] -> Board -> Board
zeroPoints [] b = b
zeroPoints ((i1, i2) : l) b = zeroPoints l $ replaceInList i1 (replaceInList i2 0 (b Unsafe.!! i1)) b

replaceInList :: Int -> a -> [a] -> [a]
replaceInList i e l = h <> (e : (Unsafe.tail t)) where (h , t) = splitAt i l
--replaceInList i _ _ = error $ show i

checkPoint :: Board -> Point -> Maybe Point
checkPoint b p@(i1, i2)
  | (b Unsafe.!! i1) Unsafe.!! i2 > 9 = Just p
  | otherwise                          = Nothing

add1ForBoard :: Board -> Board
add1ForBoard b = add1ForLine <$> b

add1ForLine :: Line -> Line
add1ForLine l = add1ForCell <$> l

add1ForCell :: Cell -> Cell
add1ForCell c = c + 1

type BoardState = (Int , Board)

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
