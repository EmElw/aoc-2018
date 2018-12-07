{-# LANGUAGE ExplicitForAll #-}

import           Data.Char
import           Data.List       as List
import           Data.List.Split
import           Data.Map        as Map hiding (filter, map)
import           Data.Maybe

readAndPrint :: FilePath -> IO ()
readAndPrint fp = do
    s <- readFile fp
    let points = parseInput s
    let area = boundary (map snd points)
    let result = evaluate area points
    print result

task6_1 :: FilePath -> IO ()
task6_1 fp = do
    s <- readFile fp
    let points       = parseInput s
    let area         = boundary (map snd points)
    let result_area  = evaluate area points
    let sum_map      = sumByClaimId result_area 
    let filter_map   = filterInfinites result_area sum_map
    let filter_list  = reverse (sortOn snd (assocs filter_map))
    print (take 10 filter_list)

task6_2 :: FilePath -> Int -> IO ()
task6_2 fp t = do
    s <- readFile fp
    let points       = parseInput s
    let area         = boundary (map snd points)
    let filter_list  = filter (\x -> (x `sumDistancesToPoints` map snd points) < t ) (concat (rows area))
    print (length filter_list)

sumDistancesToPoints :: Cell -> [Point] -> Int
sumDistancesToPoints c ps = sum (map (point c >-<) ps)

sumByClaimId :: Area -> Map Id Int
sumByClaimId a = sumByClaimId' (concat (rows a)) Map.empty

sumByClaimId' :: [Cell]-> Map Id Int -> Map Id Int
sumByClaimId' [] m = m
sumByClaimId' (x:xs) m = sumByClaimId' xs newMap
    where
        newMap
            | isNothing o = m
            | otherwise   = incAtKey o m
        o = owner x

incAtKey :: Ord k => Num a => k -> Map k a -> Map k a
incAtKey = alter f
    where
        f Nothing  = Just 1
        f (Just x) = Just (x+1)

-- | Removes all id's for which atleast on claimed cell is at the border
filterInfinites :: Area -> Map Id Int -> Map Id Int
filterInfinites a m = List.foldr (\cell m -> Map.delete (owner cell) m) m borderCells
    where
        borderCells = filter (`isOnBorder` a) (concat (rows a))

-- | Tests if a cell is on the border of a given area
isOnBorder :: Cell -> Area -> Bool
isOnBorder c a = cx <= x1 || cx >= x2 || cy <= y1 || cy >= y2
    where
        (cx,cy) = point c
        ((x1,y1), (x2,y2)) = dimensions a

type Id = Maybe Int

type Point = (Int,Int)

data Cell = Cell {point :: Point, owner :: Id, curr :: Int}
    deriving (Eq)

instance Show Cell where
    show c
        -- | owner c == Just 36              = "///"
        | isJust (owner c) && curr c == 0 = "(" ++ [chr(65 + fromJust (owner c))] ++ ")"
        | isJust (owner c)                = " " ++ [chr(65 + fromJust (owner c))] ++ " "
        | otherwise                       = " . "

data Area = Area {rows :: [[Cell]], dimensions :: (Point, Point)}
    deriving (Eq)

instance Show Area where
    show area = unlines (map (concatMap show) (rows area))

-- | Manhattan distance between two points
(>-<) :: Point -> Point -> Int
(>-<) (p1,p2) (q1,q2) = abs (p1 - q1) + abs (p2 - q2)

-- | Generate an empty area given a top left and bottom right point
empty :: Point -> Point -> Area
empty (a,b) (c,d) = Area (chunksOf width cells) ((a,b),(c,d))
    where
        width = 1 + c - a
        cells = map (\pt -> Cell pt Nothing (-1)) coords
        coords = [(x,y) | y <- [b..d], x <- [a..c]]

-- | Evaluates the claims of a list of id'd points in an area
evaluate :: Area -> [(Id, Point)] -> Area
evaluate = List.foldr f
        where f (id, pt) area = updateArea area id pt

-- | Claims cells in the area
updateArea :: Area -> Id -> Point -> Area
updateArea a id p = Area newRows (dimensions a)
    where
        newRows = map (map (`updateCell` (id, p))) (rows a)

-- | Updates a cell, given a claimant (Id, Point)
updateCell :: Cell -> (Id, Point) -> Cell
updateCell cell (id, pt)
        | curr_dist == -1        = Cell oldPt id      new_dist   -- claimant wins by default
        | new_dist  <  curr_dist = Cell oldPt id      new_dist   -- claimant wins
        | new_dist  == curr_dist = Cell oldPt Nothing new_dist   -- clash, no winner
        | otherwise              = cell                          -- no change
        where
            new_dist    = pt >-< oldPt
            curr_dist   = curr cell
            oldPt       = point cell

-- | Given a list of points, creates an area in which all points are within
boundary :: [Point] -> Area
boundary [] = error "boundary : empty input"
boundary ps = boundary' ps (head ps) (head ps)

boundary' :: [Point] -> Point -> Point -> Area
boundary' [] ab cd = Main.empty ab cd
boundary' ((p,q):xs) (a,b) (c,d) =
    boundary' xs newTl newBr
    where
        newTl = (min p a, min q b)
        newBr = (max p c, max q d)

-- | Parses a string on the specified format and gives each point an id
parseInput :: String -> [(Id, Point)]
parseInput s = parseInput' (lines s) 1

parseInput' :: [String] -> Int -> [(Id, Point)]
parseInput' [] _ = []
parseInput' (x:xs) id = new : parseInput' xs (id + 1)
    where
        new = (Just id, read ("("++x++")")) :: (Id, Point)
