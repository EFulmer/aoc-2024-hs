import Data.List
import System.IO

main :: IO ()
main = do
    putStrLn "Part One"
    contents <- readFile "day_01.txt"
    let rawLines = lines contents
    let rawInts = map words rawLines
    let parsedInts = (fmap . fmap) read rawInts :: [[Int]]
    let left = fmap (!! 0) parsedInts
    let right = fmap (!! 1) parsedInts
    let sortedLeft = sort left
    let sortedRight = sort right
    let diffs = zipWith (-) sortedLeft sortedRight
    let absDiffs = fmap abs diffs
    let answer = sum absDiffs
    putStrLn $ "Answer: " ++ (show answer)
    putStrLn "Part Two"
    let frequencies = fmap (\x -> count x sortedRight) sortedLeft
    let freqsMultiplied = zipWith (*) sortedLeft frequencies
    let answerTwo = sum freqsMultiplied
    putStrLn $ "Answer: " ++ (show answerTwo)

count :: (Eq a) => a -> [a] -> Int
count a xs = go xs 0
    where go []     acc = acc
          go (x:xs) acc = if a == x then go xs (acc+1) else go xs acc
