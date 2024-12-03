import System.IO

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)

gt :: (Ord a) => (a, a) -> Bool
gt = uncurry (>)

lt :: (Ord a) => (a, a) -> Bool
lt = uncurry (<)

isMonotonicIncreasing :: (Ord a) => [(a,a)] -> Bool
isMonotonicIncreasing = all lt

isMonotonicDecreasing :: (Ord a) => [(a,a)] -> Bool
isMonotonicDecreasing = all gt

isMonotonic :: (Ord a) => [a] -> Bool
isMonotonic xs = let xp = pairwise xs in
    isMonotonicDecreasing xp || isMonotonicIncreasing xp

inRange :: (Ord a, Num a) => (a, a) -> a -> a -> Bool
inRange (x, y) bot top = let absolute = abs (x - y) in
    bot <= absolute && absolute <= top

rangePred :: (Ord a, Num a) => (a, a) -> Bool
rangePred p = inRange p 1 3

listRange :: (Ord a, Num a) => [a] -> Bool
listRange xs = let xp = pairwise xs in
    all rangePred xp

isReportTotallySafe :: (Ord a, Num a) => [a] -> Bool
isReportTotallySafe xs = (isMonotonic xs) && (listRange xs)

withoutNth :: Int -> [a] -> [a]
withoutNth i xs = (take i xs) ++ (drop (i+1) xs)

allSublists :: [a] -> [[a]]
allSublists xs = [withoutNth i xs | i <- [0..(length xs)+1]]

safeWithOneRemoval :: (Ord a, Num a) => [a] -> Bool
safeWithOneRemoval xs = any isReportTotallySafe (allSublists xs)

partTwoSingle :: (Ord a, Num a) => [a] -> Bool
partTwoSingle x = isReportTotallySafe x || safeWithOneRemoval x

partTwo :: (Ord a, Num a) => [[a]] -> [[a]]
partTwo xs = filter (\x -> isReportTotallySafe x || safeWithOneRemoval x) xs

readAndParseInput :: (Ord a, Num a, Read a) => String -> IO [[a]]
readAndParseInput fileName = do
    rawContents <- readFile fileName
    let rawLines = lines rawContents
    let rawNums  = fmap words rawLines
    return $ (fmap . fmap) read rawNums

main :: IO ()
main = do
    putStrLn "Part One"
    reports <- readAndParseInput "./day_02.txt"
    putStrLn $ show $ length reports
    let safeReportsPartOne = filter isReportTotallySafe reports
    let numSafeReports = length safeReportsPartOne
    putStrLn $ show $ numSafeReports
    putStrLn "Part Two"
    let safeReportsPartTwo = filter (\x -> isReportTotallySafe x || safeWithOneRemoval x) reports
    putStrLn $ show $ length safeReportsPartTwo
