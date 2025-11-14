--Calculați suma pătratelor elementelor impare dintr-o listă dată ca parametru.
suma :: [Int] -> Int
suma [] = 0
suma xs = foldr (+) 0 (map (^2) (filter odd xs))


--Scrieți o funcție care verifică că toate elementele dintr-o listă sunt True, folosind foldr.
verif :: [Bool] -> Bool
verif xs = foldr (&&) True xs



--Scrieți o funcție care verifică dacă toate elementele dintr-o listă de numere întregi satisfac o proprietate dată ca parametru.
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies prop xs = foldr (\curent rest -> (prop curent) && rest) True xs



--Scrieți o funcție care verifică dacă există elemente într-o listă de numere întregi care satisfac o proprietate dată ca parametru.
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies prop xs = foldr (\curent rest -> (prop curent) || rest) False xs



--Redefiniți funcțiile map și filter folosind foldr. Le puteți numi mapFoldr și filterFoldr.
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\curent rest -> f curent : rest) [] xs

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr prop xs = foldr (\curent rest -> if prop curent then curent : rest else rest) [] xs


--Folosind funcția foldl, definiți funcția listToInt care transformă o listă de cifre (un număr foarte mare reprezentat ca listă) în numărul întreg asociat. Se presupune că lista de intrare este dată corect.
listToInt :: [Integer] -> Integer
listToInt xs = foldl (\rest curent -> rest * 10 + curent) 0 xs


--(a) Scrieți o funcție care elimină toată aparițiile unui caracter dat dintr-un șir de caractere.
rmChar :: Char -> String -> String
rmChar c xs = foldr (\curent rest -> if curent == c then rest else curent : rest) [] xs


--(b) Scrieți o funcție recursivă care elimină toate caracterele din al doilea argument care se găsesc în primul argument, folosind rmChar.
rmCharRec :: String -> String -> String
rmCharRec [] xs = xs
rmCharRec _ [] = []
rmCharRec (c:cs) xs = rmCharRec cs (rmChar c xs)

--(c) Scrieți o funcție echivalentă cu cea de la (b) care folosește însă rmChar și foldr.
rmCharFoldr :: String -> String -> String
rmCharFoldr chars xs = foldr (\curent rest -> rmChar curent rest) xs chars


--Scrieți o funcție myReverse care primește ca parametru o listă de întregi și întoarce lista elementelor în ordine inversă.
myReverse :: [Int] -> [Int]
myReverse xs = foldl (\rest curent -> curent : rest) [] xs


--Scrieți un predicat myElem care verifică apartenența unui întreg la o listă de întregi.
myElem :: Int -> [Int] -> Bool
myElem x xs = foldr (\curent rest -> (curent == x) || rest) False xs


--Scrieți o funcție myUnzip care transformă o listă de perechi într-o pereche de liste: una a componentelor de pe prima poziție, iar cealaltă a componentelor de pe a doua poziție din perechile din lista inițială.
myUnzip :: [(a,b)] -> ([a],[b])
myUnzip xs = foldr (\(a,b) (restA, restB) -> (a : restA, b : restB)) ([],[]) xs


--Scrieți o funcție union care întoarce lista reuniunii a două liste de întregi primite ca parametri.
union :: [Int] -> [Int] -> [Int]
union xs ys = foldr (\curent rest -> if myElem curent ys then rest else curent : rest) ys xs


--Scrieți o funcție intersect care întoarce lista intersecției a două liste de întregi primite ca parametri.
intersect :: [Int] -> [Int] -> [Int]
intersect xs ys = foldr (\curent rest -> if myElem curent ys then curent : rest else rest) [] xs


--Scrieți o funcție permutations care întoarce lista tuturor permutărilor elementelor unei liste de întregi primite ca parametru.
pozitioneaza :: Int -> [Int] -> [[Int]]
pozitioneaza x l = foldr (\a y -> ((take a l) ++ (x : drop a l)):y) [] [0..(length l)]


permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations (p:ps) = foldr (\x y -> (pozitioneaza p x) ++ y) [] (permutations ps)









