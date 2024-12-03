import System.IO

pairwise :: [a] -> [(a, a)]
pairwise xs = zip xs (tail xs)

gt :: (Ord a) => (a, a) -> Bool
gt = uncurry (>)

lt :: (Ord a) => (a, a) -> Bool
lt = uncurry (<)

isMonotonic :: (Ord a) => [a] -> Bool
isMonotonic xs = let xp = pairwise xs in
    all gt xp || all lt xp

inRange :: (Int, Int) -> Int -> Int -> Bool
inRange (x, y) bot top = let absolute = abs (x - y) in
    bot <= absolute && absolute <= top

rangePred :: (Int, Int) -> Bool
rangePred p = inRange p 1 3

listRange :: [Int] -> Bool
listRange xs = let xp = pairwise xs in
    all rangePred xp

fullPredicate :: [Int] -> Bool
fullPredicate xs = (isMonotonic xs) && (listRange xs)

main :: IO ()
main = do
    rawContents <- readFile "./day_02.txt"
    let rawLines = lines rawContents
    let rawNums  = fmap words rawLines
    let reports  = (fmap . fmap) read rawNums :: [[Int]]
    putStrLn "Part One"
    putStrLn $ show $ length reports
    let safeReports = filter fullPredicate reports
    putStrLn $ show $ length safeReports
