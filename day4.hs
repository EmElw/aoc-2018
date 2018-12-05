import           Data.Char
import           Data.List as List
import           Data.Map.Strict as Map
import           Data.Time
import           Data.Maybe

-- |
task4_1 :: FilePath -> IO ()
task4_1 fp = do
    s <- readFile fp
    let entries = parseInput s
    let m (x,y) = x*y
    print (m (evalTask1(evalEntries entries)))

task4_2 :: FilePath -> IO ()
task4_2 fp = do
    s <- readFile fp
    let entries = parseInput s
    let m (x,y) = x*y
    print (m (evalTask2(evalEntries entries)))

type Guard = Int
type Minute = Int
type MinFreq = Map Minute Int
type SleepSchedule = Map Guard MinFreq

-- | Evaluates a list of entries and constructs a SleepSchedule 
evalEntries :: [Entry] -> SleepSchedule
evalEntries es = evalEntries' es empty (-1)

evalEntries' :: [Entry] -> SleepSchedule -> Guard -> SleepSchedule
evalEntries' []                          map _ = map
evalEntries' ((_,Switch,Just newId):xs ) map _ = evalEntries' xs map newId
evalEntries' ((t1,Sleep,_):(t2,Wake,_):xs) map g = evalEntries' xs newMap g
    where
        newMap  = updt map g range
        range   = [(minutes t1)..(minutes t2)]

updt :: SleepSchedule -> Guard -> [Int] -> SleepSchedule
updt map guard range = newMap
    where
        newMap      = alter f guard map
        f Nothing   = Just (incMinFreqWithRange empty range)
        f (Just x)  = Just (incMinFreqWithRange x     range)

incMinFreqWithRange :: MinFreq -> [Int] -> MinFreq
incMinFreqWithRange map [] = map
incMinFreqWithRange map (x:xs) = 
    incAtMinute newMap x
    where newMap = incMinFreqWithRange map xs

-- | Increments the value by 1 at the given key, or creates a new pair (key,1)
incAtMinute :: MinFreq -> Minute -> MinFreq
incAtMinute map key = alter f key map
    where 
        f Nothing   = Just 1
        f (Just x)  = Just (x+1)
        
evalTask1 :: SleepSchedule -> (Guard, Minute)
evalTask1 m = (guard, minute - 1)
    where 
        minute      = mapMax (fromJust (Map.lookup guard m))
        guard       = mapMax sumSleep
        sumSleep    = Map.map (Map.foldr (+) 0) m 

evalTask2 :: SleepSchedule -> (Guard, Minute)
evalTask2 m = (guard, minute)
    where
        minute      = mapMax (fromJust (Map.lookup guard m))
        guard       = fst (last (sortOn snd (assocs m)))
        maxSleep    = Map.map mapMaxValue m

-- | Returns the key of the pair with the highest value
mapMax :: MinFreq -> Minute
mapMax map = fst (last (sortOn snd (assocs map)))

-- | Returns the value of the pair with the highest value
mapMaxValue :: MinFreq -> Minute
mapMaxValue map = snd (last (sortOn snd (assocs map)))


minutes :: UTCTime -> Minute
minutes t = minutes
    where TimeOfDay _ minutes _ = timeToTimeOfDay (utctDayTime t)

type Entry = (UTCTime, Event, Maybe Guard)

data State = Awake | Asleep

data Event = Wake | Sleep | Switch
    deriving (Eq, Show)

-- | Multiline input parse
parseInput :: String -> [Entry]
parseInput s = sortOn fst3 (List.map parseEntry (lines s))

fst3 (a,b,c) = a

-- | Single line input parse
parseEntry :: String -> Entry
parseEntry s = (parseTime (cutTime s), evt, guard)
    where
        (evt,guard) = parseEvent(cutEvent s)
        cutTime     = drop 1 . take 17
        cutEvent    = drop 19
        timeFormat  = "%0Y-%0m-%0d %0H:%0M"
        parseTime s = parseTimeOrError True defaultTimeLocale timeFormat s :: UTCTime

-- | Parses the following types of input strings
-- falls asleep
-- wakes up
-- Guard #2411 begins shift
parseEvent :: String -> (Event, Maybe Int)
parseEvent "falls asleep" = (Sleep, Nothing)
parseEvent "wakes up"     = (Wake, Nothing)
parseEvent  s             = (Switch, Just id)
    where
        id      = read numb :: Int
        numb    = List.filter isDigit s
