module Day15 where

import qualified Data.Map      as Map
import qualified Data.Set      as Set

import qualified Relude.Unsafe as Unsafe

--import qualified Data.List     as L
import qualified Data.Text     as T

import qualified Data.List.Extra as L

day13 :: IO ()
day13 = do
--  t <- readFileText "input/i15" -- NNCB
--  t <- readFileText "input/i15" --17
--  t <- readFileText "input/in15" --19 --103
  t <- readFileText "input/input15" -- SHPPPVOFPBFCHHBKBNCV
--  putTextLn $ "day 15 \n" <> (show $ (run2 "NNCB" t))
--  putTextLn $ "day 15 \n" <> (show $ (run2 "SHPPPVOFPBFCHHBKBNCV" t))
  putTextLn $ "day 15 \n" <> (show $ (run2 "SHPPPVOFPBFCHHBKBNCV" t))

type Return = Int
--type Return = StateMap
--type Return = [(CharPair, Int)]
--type Return = [Int]
--type Return = [(Char, Int)]
--type Return = Map Char Int

run2 :: Text -> Text -> Return
--run2 start t = count $ Map.toList $ step2 40 (buildStateMap $ toString start) (buildMatchMap t)
run2 start t = count' $ sort $ snd <$> correctLast <$> Map.toList (Map.map sum $ buildMap $ ((\ ((c1 , _), i ) -> (c1, i)) <$> (Map.toList $ step2 40 (buildStateMap $ toString start) (buildMatchMap t))))

correctLast :: (Char , Int) -> (Char , Int)
correctLast ('V' , i) = ('V' , i +1)
correctLast a         = a


step2 :: Int -> StateMap -> MatchMap -> StateMap
step2 0 s _        = s
step2 i s matchMap = step2 (i - 1)  s' matchMap
  where s' = nextState2 matchMap s

nextState2 :: MatchMap -> StateMap -> StateMap
nextState2 matchMap s = Map.map sum (buildMap $ concat (nextState2' matchMap <$> (Map.toList s)))

nextState2' :: MatchMap -> CharPairWithCount -> [CharPairWithCount]
nextState2' matchMap ((c1 , c2) , i) = [((c1 , c), i) , ((c , c2), i)]
 where c = match2chars matchMap c1 c2

buildStateMap :: String -> StateMap
buildStateMap s = Map.fromList $ sortGroupAndCount $ buildStateMap' s

buildStateMap' :: String -> [CharPair]
buildStateMap' [] = []
buildStateMap' [_] = []
buildStateMap' (c1 : c2 : s) = (c1 , c2) : (buildStateMap' (c2 : s))


type StateMap = Map CharPair Int

type CharPairWithCount = (CharPair , Int)

type CharPair = (Char , Char)

run1 :: Text -> Text -> Int
run1 start t = count $ step 10 (toString start) (buildMatchMap t)

count :: (Ord a ) => [a] -> Int
count l = count' $ sort (length <$> sortAndGroup l)

count' :: [Int] -> Int
count' l = (Unsafe.last l) - (Unsafe.head l)

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
