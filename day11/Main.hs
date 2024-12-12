-- problem modeling
data Outcome = One Int | Two (Int, Int) deriving Show

numDigits :: Int -> Int
numDigits = length . show

evenNumDigits :: Int -> Bool
evenNumDigits = even . numDigits

blinkOne :: Int -> Outcome
blinkOne x
    | x == 0          = One 1
    | evenNumDigits x = Two $ splitNumber x
    | otherwise       = One $ x * 2024

splitNumber :: Int -> (Int, Int)
splitNumber x = (read (take half stringRep), read (drop half stringRep))
    where
        stringRep = show x
        half      = (length stringRep) `div` 2

blinkAll :: [Int] -> [Outcome]
blinkAll = fmap blinkOne

collectResults :: [Outcome] -> [Int]
collectResults os =
    go [] os
    where
        go acc ((One o):os)        = go (o:acc) os
        go acc ((Two (o1, o2)):os) = go (o2:o1:acc) os -- note the o2:o1!
        go acc []                  = reverse acc

blinkRound :: [Int] -> [Int]
blinkRound = collectResults . blinkAll

getIteration :: [a] -> Int -> [a]
getIteration xs n = (iterate blinkRound xs) !! n

-- input reading
readInput :: String -> IO [Int]
readInput fileName = do
    rawContents <- readFile fileName
    return $ fmap read (words rawContents)

main :: IO ()
main = do
    start <- readInput "input/day_11.txt"
    let answer = length $ getIteration start 75
    putStrLn $ show answer
