data Tree = Empty  -- arbore vid
  | Node Int Tree Tree Tree -- arbore cu valoare de tip Int in radacina
                            -- si 3 fii
  
extree :: Tree
extree = Node 4 (Node 5 Empty Empty Empty) 
                (Node 3 Empty Empty (Node 1 Empty Empty Empty)) Empty

--1. Instanțiați clasa următoare pentru tipul Tree.

class ArbInfo t where
  level :: t -> Int -- intoarce inaltimea arborelui; 
                    -- consideram ca un arbore vid are inaltimea 0
  sumval :: t -> Int -- intoarce suma valorilor din arbore
  nrFrunze :: t -> Int -- intoarce nr de frunze al arborelui


instance ArbInfo Tree where
    level Empty = 0
    level (Node _ st mij dr) = 1 + max (level st) (max (level mij) (level dr))
    sumval Empty = 0
    sumval (Node rad st mij dr) = rad + sumval st + sumval mij + sumval dr
    nrFrunze Empty = 0
    nrFrunze (Node _ Empty Empty Empty) = 1
    nrFrunze (Node _ st mij dr) = nrFrunze st + nrFrunze mij + nrFrunze dr


--2. Instanțiați clasa Scalar folosindu-vă de tipuri primitive (hint: nu uitați, trebuie să fie corpuri comutative). Apoi, considerați clasa de mai jos a vectorilor.
class Scalar a where
  zero :: a 
  one :: a 
  adds :: a -> a -> a
  mult :: a -> a -> a
  negates :: a -> a
  recips :: a -> a


instance Scalar Float where
    zero = 0.0
    one = 1.0
    adds x y = x + y
    mult x y = x * y
    negates x = -x
    recips x = 1.0 / x


--3. Scrieți două instanțe ale clasei Vector pentru a reprezenta vectori bidimensionali și tridimensionali.

data Vector2 a = Vector2 a a
  deriving (Show, Eq)


data Vector3 a = Vector3 a a a
    deriving (Show, Eq)


class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector


instance (Scalar a) => Vector Vector2 a where
    zerov = Vector2 zero zero
    onev = Vector2 one one
    addv (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (adds x1 x2) (adds y1 y2)
    smult s (Vector2 x y) = Vector2 (mult s x) (mult s y)
    negatev (Vector2 x y) = Vector2 (negates x) (negates y)


instance (Scalar a) => Vector Vector3 a where
    zerov = Vector3 zero zero zero
    onev = Vector3 one one one
    addv (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (adds x1 x2) (adds y1 y2) (adds z1 z2)
    smult s (Vector3 x y z) = Vector3 (mult s x) (mult s y) (mult s z)
    negatev (Vector3 x y z) = Vector3 (negates x) (negates y) (negates z)

