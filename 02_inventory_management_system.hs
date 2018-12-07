import Data.List

printCheckSum :: FilePath -> IO ()
printCheckSum fp = do
    s <- readFile fp
    print (calcChecksum (parseInput s))
    
printBoxID :: FilePath -> IO ()
printBoxID fp = do
    s <- readFile fp
    print (getBoxID (parseInput s))

getBoxID :: [String] -> String
getBoxID [] = error "getBoxID : no match"
getBoxID [_] = error "getBoxID : no match"
getBoxID (x:xs) = 
    if null res
    then getBoxID xs
    else head res
        where
        res = filter (\list -> length list == (length x -1)) commons
        commons = map (getCommons x) xs

-- | Returns the matching elements
-- i.e. "abcdef"
--                      => "abcde"
--      "abcdeg"
getCommons :: String -> String -> String
getCommons a b = map fst (filter (uncurry (==)) (zip a b))


parseInput :: String -> [String]
parseInput = lines 

calcChecksum :: [String] -> Int
calcChecksum inp =
    twos * threes
    where 
        twos = length (filter fst res)
        threes = length (filter snd res) 
        res = map exactlyTwoOrThreeOccurences inp

exactlyTwoOrThreeOccurences :: String -> (Bool, Bool)
exactlyTwoOrThreeOccurences s =
    (2 `elem` lenList, 3 `elem` lenList)
    where lenList = map length ((group . sort) s)