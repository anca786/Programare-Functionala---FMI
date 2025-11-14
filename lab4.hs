factori :: Int -> [Int]
factori n = [x | x<- [1..n `div` 2], n `mod` x ==0 ] ++ [n]


prim :: Int -> Bool
prim n
    |n<2 = False
    |otherwise = factori n == [1,n] 

numerePrime :: Int -> [Int]
numerePrime n =[x | x <- [2..n], prim x]


myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myzip3 xs ys zs



firstE1 :: [(a,b)] -> [a]
firstE1 xs = map fst xs


sumList :: [[Int]] -> [Int]
sumList xs = map sum xs 


prel2 :: [Int] -> [Int]
prel2 = map (\x -> if even x then x `div` 2 else x * 2)


contine :: Char -> [String] -> [String]
contine c xs = filter(elem c) xs


patrate :: [Int] -> [Int]
patrate xs = map (\x ->x*x) (filter odd xs) 


pozitiiImpare :: [Integer] -> [Integer]
pozitiiImpare xs = map (\(_,x) -> x*x) (filter (\(i,_) -> odd i) (zip [1..] xs))


numaiVocale :: [[Char]] -> [[Char]]
numaiVocale = map (filter(`elem` "aeiouAEIOU") )


mymap :: (a -> b) -> [a] -> [b]
mymap f l = [ f x | x <- l]

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs) = if f x then x : myfilter f xs else myfilter f xs