import Data.Char

verifl :: [Int] -> Bool
verifl xs = even(length xs)


takefinal :: [Int] -> Int -> [Int]
takefinal xs n =
    if length xs < n 
        then xs
        else
            drop (length xs - n) xs


remove :: [Int] -> Int -> [Int]
remove xs n = take (n-1) xs ++ drop n xs


semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
    |even h = h `div`2 : t'
    |otherwise = t'
    where t' = semiPareRec t

myreplicate :: Int -> a -> [a]
myreplicate n v
    |n == 0 = []
    |otherwise = v : myreplicate (n-1) v


sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x:xs)
    |odd x = x + sumImp xs
    |otherwise = sumImp xs



totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x:xs)
    |head x == 'A' = length x + totalLen xs
    |otherwise = totalLen xs



palindrom :: String -> Bool
palindrom s = s == reverse s

vocale :: String -> Int
vocale [] = 0
vocale (x:xs) 
    |x `elem` "aeiouAEIOU" = 1 + vocale xs
    |otherwise = vocale xs


nrVocale :: [String] -> Int
nrVocale [] =0
nrVocale (x:xs) =
    if palindrom x
        then vocale x + nrVocale xs
        else nrVocale xs



f :: Int -> [Int] -> [Int]
f _ [] = []
f n (x:xs)
    |even x = x : n : f n xs
    |otherwise = x : f n xs



divizori :: Int -> [Int]
divizori n = [d | d <- [1..n], n `mod` d == 0]


listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv (x:xs) = divizori x : listadiv xs


listadiv2 :: [Int] -> [[Int]]
listadiv2 xs = [divizori x | x <- xs]


inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec i s (x:xs)
    |x >= i && x <= s = x : inIntervalRec i s xs
    |otherwise = inIntervalRec i s xs


inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp i s xs = [x | x<- xs, x >= i && x <=s]



pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs)
    |x > 0 = 1 + pozitiveRec xs
    |otherwise = pozitiveRec xs

pozitiveComp :: [Int] -> Int
pozitiveComp xs = length [x | x<- xs, x > 0]


pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec x = aj(zip [0..] x)
    where
        aj :: [(Int, Int)] -> [Int]
        aj [] = []
        aj ((i,n) : xs)
            |odd n = i : aj xs
            |otherwise = aj xs


pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp xs = [i | (i,n) <- zip [0..] xs, odd n]



multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
    |isDigit x = digitToInt x * multDigitsRec xs
    |otherwise = multDigitsRec xs 


multDigitsComp :: String -> Int
multDigitsComp xs = product [digitToInt x | x<- xs, isDigit x]









