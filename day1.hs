import Data.Set hiding (map, filter)


frequencySum :: [Int] -> Int
frequencySum = sum 

findFrequency :: FilePath -> IO ()
findFrequency fp = do 
    s <- readFile fp
    let nums = parseInput s
    print (sum nums)

findRepeatingFrequency :: FilePath -> IO ()
findRepeatingFrequency fp = do
    s <- readFile fp
    let nums = parseInput s
    print (findRepeat nums)

findRepeat :: [Int] -> Int
findRepeat xs = 
    findRepeat' xs (singleton 0) 0

findRepeat' :: [Int] -> Set Int -> Int -> Int
findRepeat' (x:xs) set sum
    | newSum `member` set = newSum
    | otherwise = findRepeat'   (xs ++ [x]) 
                                (insert newSum set) 
                                newSum
        where newSum = sum + x
     

parseInput :: String -> [Int]
parseInput s = map read (lines (filter (/= '+') s)) :: [Int]