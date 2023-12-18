import Data.Char
import Data.List

main:: IO()
main = do
    print (findMaxPalindrome 1112332)
    print (findMaxPalindrome 22220)
    print (findMaxPalindrome 2205)
    print (findMaxPalindrome 120021)
    print (findMaxPalindrome 12320)
    print (findMaxPalindrome 123)

    print (calculate "1+2+x" [('x', 5)])
    print (calculate "x+2+x-2+y+z" [('x', 1), ('y', 2), ('z', 3)])
    print (calculate "x+2-x-2+y+x" [('x', 1), ('y', -15)])
    print (calculate "y+2+x-2+z+z+z+x+5" [('x', 1), ('y', 2), ('z', 3)])
    print (calculate "8-2" [])
    print (calculate "5" [])
    


   
    
--Task 1

-- Algorithm: convert int into list -> order list by descending order -> find left, middle, and right ->
-- concatenate into one list -> convert to int -> print

findMaxPalindrome :: Int -> Int
findMaxPalindrome x 
    | x < 10 = x
    | otherwise = buildPalindrome (dSort (toList x))

buildPalindrome :: [Int] -> Int
buildPalindrome x = listToInt (concat [findLeft x, findMiddle x, findRight x])

findLeft :: [Int] -> [Int]
findLeft [] = []
findLeft [x] = []
findLeft (x:y:xs)
    | x == y = x:findLeft xs
    | otherwise = findLeft ([y] ++ xs)

findMiddle :: [Int] -> [Int]
findMiddle [] = []
findMiddle [x] = [x]
findMiddle (x:y:xs)
    | x == y = findMiddle xs
    | otherwise = [x]

findRight :: [Int] -> [Int]
findRight [] = []
findRight [x] = []
findRight (x:y:xs)
    | x == y = findRight xs ++ [x]
    | otherwise = findRight ([y] ++ xs)

toList :: Int -> [Int]
toList x
    | x < 10 = [x]
    | otherwise = toList (div x 10) ++ [mod x 10]

listToInt :: [Int] -> Int
listToInt = foldl (\num x -> num * 10 + x) 0

dSort :: [Int] -> [Int]
dSort [] = []
dSort (x:xs) = step x (dSort xs)

step :: Int -> [Int] -> [Int]
step x [] = [x]
step x (y:ys)
    | x >= y = x:(y:ys)
    | otherwise = y : step x ys

-- End of task 1

-- Task 2

-- Algorithm: replace all variables with their values -> calculate the expression

calculate :: String -> [(Char, Int)] -> Int
calculate str xs
    | length str == 1 = ord (str !! 0) - ord '0'
    | otherwise = calculateClean (cleanUp str xs)

replaceVariable :: Char -> [(Char, Int)] -> Char
replaceVariable ch (x:xs)
    | ch == fst x = chr (snd x + ord '0')
    | otherwise = replaceVariable ch xs

cleanUp :: String -> [(Char, Int)] -> String
cleanUp str [] = str
cleanUp str xs = helper 0 ""
    where
        helper pos res
            | pos == (length str) - 1 = if not (isDigit (str !! pos)) then res ++ [replaceVariable (str !! pos) xs]
                                        else res ++ [(str !! pos)]
            | (str !! pos) == '+' = helper (pos + 1) (res ++ ['+'])
            | (str !! pos) == '-' = helper (pos + 1) (res ++ ['-'])
            | isDigit (str !! pos) = helper (pos + 1) (res ++ [(str !! pos)])
            | otherwise = helper (pos + 1) (res ++ [(replaceVariable (str !! pos) xs)])

calculateClean :: String -> Int
calculateClean str = helper 1 (ord (str !! 0) - ord '0') 
    where
        helper pos res
            | pos == (length str) - 2 = if str !! pos == '+' then res + (ord (str !! (pos + 1)) - ord '0')
                                        else res - (ord (str !! (pos + 1)) - ord '0')
            | otherwise = 
                if str !! pos == '+' then helper (pos + 2) (res + (ord (str !! (pos + 1)) - ord '0'))
                else helper (pos + 2) (res - (ord (str !! (pos + 1)) - ord '0'))