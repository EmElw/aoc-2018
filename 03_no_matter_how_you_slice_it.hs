import           Data.Char
import           Data.List       as List
import           Data.List.Split
import           Data.Map        as Map hiding (filter, map, null)
import           Data.Set        as Set hiding (filter, map, null)
import           Text.Printf

-- | Task 3.1: find the number of squares claimed by at least 2 agents
-- strategy: crude implementation that groups all claimed points in a list
-- and then only keeps those with length > 1
task3_1 :: FilePath -> IO ()
task3_1 fp = do
    s <- readFile fp
    print (getOverlap (parseInput s))

data Rectangle =
    Rectangle {i :: Int, x :: Int, y :: Int, w :: Int, h :: Int}
    deriving (Eq)

type Point = (Int, Int)

instance Show Rectangle where
    show r = printf "#%d @ %d,%d: %dx%d \n" (i r) (x r) (y r) (w r) (h r)

stringToRectangle :: String -> Rectangle
stringToRectangle s = intsToRectangle (map read ls)
    where ls = filter (not.null) (splitWhen (not.isDigit) s)

intsToRectangle :: [Int] -> Rectangle
intsToRectangle [i, x, y, w, h] = Rectangle i x y w h

getOverlap :: [Rectangle] -> Int
getOverlap rects = length filterPs
    where
        filterPs = filter (\list -> length list > 1) sortGroupPs
        sortGroupPs = group(sort ps)
        ps = concatMap rectToPoints rects

rectToPoints :: Rectangle -> [Point]
rectToPoints r =
    [(x,y) | y<-ys, x<-xs]
    where
        xs = [(x r)..(x r + w r - 1)]
        ys = [(y r)..(y r + h r - 1)]

parseInput :: String -> [Rectangle]
parseInput s =
    map stringToRectangle (lines s)

-- | Task 3.2: find a rectangle that doesn't overlap ANY other triangle
-- strategy: test collision for each combination of 2 rectangles
task3_2 :: FilePath -> IO ()
task3_2 fp = do
    s <- readFile fp
    print (getUncontestedRect (parseInput s))

getUncontestedRect :: [Rectangle] -> Rectangle
getUncontestedRect [] = error "getUncontestedRect : no solution"
getUncontestedRect (r:rs) =
    if any (isOverlapping r) rs
    then getUncontestedRect (rs++[r])
    else r

isOverlapping :: Rectangle -> Rectangle -> Bool
isOverlapping a b =
    x a       < x b + w b    &&
    x a + w a > x b          &&
    y a       < y b + h b    &&
    y a + h a > y b
