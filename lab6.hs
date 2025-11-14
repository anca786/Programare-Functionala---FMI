data Fruct
    =Mar String Bool
    |Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
cosFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]


--1.a) Scrieți un predicat care verifică dacă un fruct este o portocală de Sicilia. Soiurile de portocale din Sicilia sunt Tarocco, Moro și Sanguinello.
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala soi _) 
    |soi == "Tarocco" || soi == "Moro" || soi == "Sanguinello" = True
    |otherwise = False



--1.b) Scrieți o funcție care calculează numărul total de felii ale portocalelor de Sicilia dintr-o listă de fructe.
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia xs = foldr (+) 0 (map(\(Portocala _ felii) -> felii) (filter ePortocalaDeSicilia xs))


--1.c) Scrieți o funcție care calculează numărul de mere care au viermi dintr-o listă de fructe.
areViermi :: Fruct -> Bool
areViermi (Mar _ True) = True
areViermi _ = False


nrMereViermi :: [Fruct] -> Int
nrMereViermi xs = length (filter areViermi xs)



type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

--2. a) Scrieți o funcție care întoarce "Meow!" pentru pisică și "Woof!" pentru câine.
vorbeste :: Animal -> String
vorbeste (Pisica _ ) = "Meow!"
vorbeste (Caine _ _ ) = "Woof!"



--2.b) Reamintiți-vă tipul de date predefinit Maybe.Scrieți o funcție care întoarce rasa unui câine dat ca parametru sau Nothing dacă parametrul este o pisică.
rasa :: Animal -> Maybe String
rasa (Pisica _ ) = Nothing
rasa (Caine _ r ) = Just r


data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show


--3.a) Scrieți o funcție care verifică dacă suma elementelor de pe fiecare linie este egală cu o valoare dată n. Rezolvați cerința folosind foldr.
verifica :: Matrice -> Int -> Bool
verifica (M linii) n = foldr (\(L xs) acc -> sum xs == n && acc) True linii 


--3.b) Scrieți o funcție doarPozN care are ca parametri un element de tip Matrice și un număr întreg n, și care verifică dacă toate liniile de lungime n din matrice au numai elemente strict pozitive.

doarPozN :: Matrice -> Int -> Bool
doarPozN (M linii) n = foldr (\(L xs) acc ->
        if length xs == n
        then all (>0) xs && acc
        else acc) True linii

--3.c) Definiți predicatul corect care verifică dacă toate liniile dintr-o matrice au aceeași lungime.
corect :: Matrice -> Bool
corect (M (L xs : linii)) = foldr(\(L ys) acc -> length xs == length ys && acc) True linii


