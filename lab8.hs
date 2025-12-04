import Prelude hiding (lookup)

class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert 
    :: Ord key 
    => key -> value -> c key value -> c key value
  lookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value


    --1. Adăugați definiții implicite (folosind celelalte funcții din clasă) pentru keys, values și fromList.
  keys = map fst . toList
  values = map snd . toList
  fromList = foldr (\(k, v) acc -> insert k v acc) empty


--2.Faceți PairList instanță a clasei Collection.
newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k, v)]
    insert k v (PairList pairs) = PairList ((k, v) : filter (\(key, _) -> key /= k) pairs)
    lookup k (PairList pairs) = lookupKey k pairs where 
                                lookupKey _ [] = Nothing
                                lookupKey key ((k,v):xs)
                                    | key == k = Just v
                                    | otherwise = lookupKey key xs
    delete k (PairList pairs) = PairList (filter (\(key, _) -> key /= k) pairs)
    toList = getPairList

myPairList :: PairList String Int
myPairList = PairList [("a", 1), ("b", 2), ("c", 3)]

--3.Faceți SearchTree instanță a clasei Collection.
data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare
      deriving Show

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty
    insert k v Empty = singleton k v
    insert k v (BNode left key val right)
        | k == key = BNode left key (Just v) right
        | k < key  = BNode (insert k v left) key val right
        | k > key  = BNode left key val (insert k v right)
    lookup _ Empty = Nothing
    lookup k (BNode left key val right)
        | k == key = val
        | k < key  = lookup k left
        | k > key  = lookup k right
    delete _ Empty = Empty
    delete k (BNode left key val right)
        | k == key = BNode left key Nothing right
        | k < key  = BNode (delete k left) key val right
        | k > key  = BNode left key val (delete k right)
    toList Empty = []
    toList (BNode left key val right) =
        case val of
            Nothing -> toList left ++ toList right
            Just v  -> toList left ++ [(key, v)] ++ toList right


data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a


--4.Scrieți o instanță a clasei Show pentru tipul de date Punct, astfel încât lista coordonatelor să fie afișată ca tuplu.
instance Show Punct where
    show (Pt []) = show ()
    show (Pt (x:xs)) = "(" ++ show x ++ show' xs where
                       show' [] = ")"
                       show' (x:xs) = "," ++ show x ++ show' xs


--5.Scrieți o instanță a clasei ToFromArb pentru tipul de date Punct astfel încât lista coordonatelor punctului să coincidă cu frontiera arborelui.
instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N left right) = Pt ((justList (fromArb left)) ++ (justList (fromArb right))) where
                            justList (Pt l) = l



data Geo a = Square a | Rectangle a a | Circle a
    deriving Show


class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a


--6.Instanțiați clasa GeoOps pentru tipul de date Geo. Hint: pentru valoarea pi puteți folosi funcția cu același nume (pi).
instance GeoOps Geo where
    perimeter (Square a) = 4 * a
    perimeter (Rectangle a b) = 2 * (a + b)
    perimeter (Circle r) = 2 * pi * r
    area (Square a) = a * a
    area (Rectangle a b) = a * b
    area (Circle r) = pi * r * r


--7.Instanțiați clasa Eq pentru tipul de date Geo, astfel încât două figuri geometrice să fie egale dacă au perimetrul egal.
instance (Floating a, Eq a) => Eq (Geo a) where
    g1 == g2 = perimeter g1 == perimeter g2
