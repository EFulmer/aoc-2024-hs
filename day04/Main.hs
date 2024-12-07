import qualified Data.Map as M

type Coordinates = (Int, Int)
type SearchSpace = M.Map Coordinates Char

-- transform list into a list of (x-y coordinate, value) type pairs
nestedEnumerate :: [[a]] -> [((Int, Int), a)]
nestedEnumerate xs = [
    ((i, j), xs !! i !! j) | i <- [ 0 .. (length xs)        - 1 ],
                           j <-   [ 0 .. (length (xs !! i)) - 1 ]]

makeSearchSpace :: [[Char]] -> SearchSpace
makeSearchSpace = M.fromList . nestedEnumerate

readInput :: String -> IO [String]
readInput fileName = do
    rawContents <- readFile fileName
    let rawLines = lines rawContents
    return rawLines

readAndProcessInput :: String -> IO SearchSpace
readAndProcessInput fileName = do
    rawLines <- readInput fileName
    (return . makeSearchSpace) rawLines

getXPositions :: SearchSpace -> [Coordinates]
getXPositions ss = M.keys $ M.filter (== 'X') ss

getGoingUp :: Coordinates -> [Coordinates]
getGoingUp (x, y) = [(x, y - i) | i <- [1..3]]

getGoingDown :: Coordinates -> [Coordinates]
getGoingDown (x, y) = [(x, y + i) | i <- [1..3]]

getGoingLeft :: Coordinates -> [Coordinates]
getGoingLeft (x, y) = [(x - i, y) | i <- [1..3]]

getGoingRight :: Coordinates -> [Coordinates]
getGoingRight (x, y) = [(x + i, y) | i <- [1..3]]

getDownRightDiagonal :: Coordinates -> [Coordinates]
getDownRightDiagonal (x, y) = [(x + i, y + i) | i <- [1..3]]

getDownLeftDiagonal :: Coordinates -> [Coordinates]
getDownLeftDiagonal (x, y) = [(x - i, y + i) | i <- [1..3]]

getUpRightDiagonal :: Coordinates -> [Coordinates]
getUpRightDiagonal (x, y) = [(x + i, y - i) | i <- [1..3]]

getUpLeftDiagonal :: Coordinates -> [Coordinates]
getUpLeftDiagonal (x, y) = [(x - i, y - i) | i <- [1..3]]

permutations :: [Coordinates -> [Coordinates]]
permutations = [getGoingUp, getGoingDown, getGoingLeft, getGoingRight, getDownRightDiagonal, getDownLeftDiagonal, getUpRightDiagonal, getUpLeftDiagonal]

getAllPossibleWordCoordinates :: Coordinates -> [[Coordinates]]
getAllPossibleWordCoordinates cs = [ func cs | func <- permutations ]

getWord :: [Coordinates] -> SearchSpace -> [Maybe Char]
getWord cs s = [ M.lookup k s | k <- cs ]

isValid :: [Maybe Char] -> Bool
isValid [Just 'M', Just 'A', Just 'S'] = True
isValid _                              = False

-- CheckPos checks all possible adjacent sequences of length 3 and returns the number of which make "XMAS"
checkPos :: Coordinates -> SearchSpace -> Int
checkPos c s = length $ filter isValid $ fmap (\x -> getWord x s) (getAllPossibleWordCoordinates c)

part1Logic :: SearchSpace -> Int
part1Logic s = let xPositions = getXPositions s in
    sum $ fmap (\x -> checkPos x s) xPositions

-- Begin Part Two functions.

getAPositions :: SearchSpace -> [Coordinates]
getAPositions ss = M.keys $ M.filter (== 'A') ss

-- could be a tuple instead of a list, since I only ever want five coordinates
getXCoordinates :: Coordinates -> [Coordinates]
getXCoordinates (x, y) = [(x - 1, y - 1), (x + 1, y + 1), (x, y), (x + 1, y - 1), (x - 1, y + 1)]

getXWord :: Coordinates -> SearchSpace -> [Maybe Char]
getXWord c s = let xc = getXCoordinates c in
    getWord xc s

isValidPartTwo :: [Maybe Char] -> Bool
isValidPartTwo [Just 'M', Just 'S', Just 'A', Just 'M', Just 'S'] = True
isValidPartTwo [Just 'M', Just 'S', Just 'A', Just 'S', Just 'M'] = True
isValidPartTwo [Just 'S', Just 'M', Just 'A', Just 'M', Just 'S'] = True
-- above three are all prob needed
isValidPartTwo [Just 'S', Just 'M', Just 'A', Just 'S', Just 'M'] = True
isValidPartTwo _                                                  = False

checkOneXPartTwo :: Coordinates -> SearchSpace -> Bool
checkOneXPartTwo c s = isValidPartTwo $ getXWord c s

count :: (Eq a) => a -> [a] -> Int
count a xs = go xs 0
    where go []     acc = acc
          go (x:xs) acc = if a == x then go xs (acc+1) else go xs acc

partTwoLogic :: SearchSpace -> Int
partTwoLogic s = let aPositions = getAPositions s in
    count True $ fmap (\c -> checkOneXPartTwo c s) aPositions

main :: IO ()
main = do
    searchSpace <- readAndProcessInput "input/day_04.txt"
    putStrLn "Part One:"
    let part1Answer = part1Logic searchSpace
    putStrLn $ "Answer: " ++ (show part1Answer)
    putStrLn "Part Two:"
    let partTwoAnswer = partTwoLogic searchSpace
    putStrLn $ "Answer: " ++ (show partTwoAnswer)
