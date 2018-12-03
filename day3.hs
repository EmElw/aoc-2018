import Text.Printf
import Data.List as List
import Data.List.Split
import Data.Char
import Data.Set as Set hiding (map, filter, null)
import Data.Map as Map hiding (map, filter, null)


task3_1 :: FilePath -> IO ()
task3_1 fp = do
    s <- readFile fp
    print (getOverlap s)

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


getOverlap :: String -> Int
getOverlap s = length filterPs
    where 
        filterPs = filter (\list -> length list > 1) sortGroupPs
        sortGroupPs = group(sort ps)
        ps = concatMap rectToPoints rects
        rects = parseInput s

rectToPoints :: Rectangle -> [Point]
rectToPoints r = 
    [(x,y) | y<-ys, x<-xs]
    where
        xs = [(x r)..(x r + w r - 1)]
        ys = [(y r)..(y r + h r - 1)]

parseInput :: String -> [Rectangle]
parseInput s = 
    map stringToRectangle (lines s)

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
    x a          <   x b + w b    &&
    x a + w a    >   x b          &&
    y a          <   y b + h b    &&
    y a + h a    >   y b