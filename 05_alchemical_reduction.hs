import           Data.Char
import           Data.List

task5_1 :: FilePath -> IO ()
task5_1 fp = do
    s <- readFile fp
    print (length (reaction s))

task5_2 :: FilePath -> IO ()
task5_2 fp = do
    s <- readFile fp
    let all = units s
    let result = zip all (map (optimizationValue s) all)
    let best = head (sortOn snd result)
    print best

optimizationValue :: String -> Char -> Int
optimizationValue inp c =
    length (reaction (filter (\x -> toLower x /= c) inp))

units :: String -> String
units s = nub $ map toLower s

reaction :: String -> String
reaction s = reaction' s []

reaction' :: String -> String -> String
reaction' [] stack = reverse stack
reaction' (x:xs) []    = reaction' xs [x]
reaction' (x:xs) (y:ys)
    | x `reactsWith` y = reaction' xs ys
    | otherwise        = reaction' xs (x:y:ys)


reactsWith :: Char -> Char -> Bool
reactsWith a b
    | caseA     == caseB        = False -- no reaction, same case
    | toLower a == toLower b    = True    -- reaction, same letter
    | otherwise                 = False
    where
        caseA = isUpper a
        caseB = isUpper b
