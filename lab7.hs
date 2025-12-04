data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)


instance Show Expr where
  show (Const x) = show x
  show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
  show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"

--1. Scrieți o funcție evalExp :: Expr -> Int care evaluează o expresie determinând valoarea acesteia.
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

--2. Scrieți o funcție evalArb :: Tree -> Int care evaluează o expresie modelată sub formă de arbore, determinând valoarea acesteia.
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add t1 t2) = evalArb t1 + evalArb t2
evalArb (Node Mult t1 t2) = evalArb t1 * evalArb t2


arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1
test22 = evalArb arb2 
test23 = evalArb arb3 
test24 = evalArb arb4 


--3.Scrieți o funcție expToArb :: Expr -> Tree care transformă o expresie în arborele corespunzător.
expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)



data IntSearchTree value
  = Empty
  | BNode
      (IntSearchTree value)     -- elemente cu cheia mai mica
      Int                       -- cheia elementului
      (Maybe value)             -- valoarea elementului
      (IntSearchTree value)     -- elemente cu cheia mai mare


--4.Scrieți o funcție lookup' de căutare a unui element într-un arbore.
lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' _ Empty = Nothing
lookup' k (BNode left key val right)
  | k == key = val
  | k < key  = lookup' k left
  | k > key  = lookup' k right

  --5. Scrieți o funcție care întoarce lista cheilor nodurilor dintr-un arbore de căutare.
keys :: IntSearchTree value -> [Int]
keys Empty = []
keys (BNode left key _ right) = keys left ++ [key] ++ keys right

--6.Scrieți o funcție care întoarce lista valorilor nodurilor dintr-un arbore de căutare.
values :: IntSearchTree value -> [value]
values Empty = []
values (BNode left _ val right) =
    case val of
        Nothing -> values left ++ values right
        Just v  -> values left ++ [v] ++ values right

--7.Scrieți o funcție de adăugare a unui element într-un arbore de căutare.
insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert k v Empty = BNode Empty k (Just v) Empty
insert k v (BNode left key val right)
  | k == key = BNode left key (Just v) right
  | k < key  = BNode (insert k v left) key val right
  | k > key  = BNode left key val (insert k v right)


--8.Scrieți o funcție care șterge (marchează ca șters) un element dintr-un arbore de căutare.
delete :: Int -> IntSearchTree value -> IntSearchTree value
delete _ Empty = Empty
delete k (BNode left key val right)
  | k == key = BNode left key Nothing right
  | k < key  = BNode (delete k left) key val right
  | k > key  = BNode left key val (delete k right)


--9.Scrieți o funcție care întoarce lista elementelor dintr-un arbore de căutare. Hint: atenție la Maybe!
toList :: IntSearchTree value -> [(Int, value)]
toList Empty = []
toList (BNode left key val right) =
    case val of
        Nothing -> toList left ++ toList right
        Just v  -> toList left ++ [(key, v)] ++ toList right

--10.Scrieți o funcție care să construiască un arbore dintr-o listă de perechi cheie-valoare.
fromList :: [(Int, value)] -> IntSearchTree value
fromList [] = Empty
fromList ((k, v):xs) = insert k v (fromList xs)


--11.Scrieți o funcție care să producă o reprezentare liniară (șir de caractere) a structurii arborescente de chei (ignorând valorile).
printTree :: IntSearchTree value -> String
printTree Empty = ""
printTree (BNode left key _ right) =
    "(" ++ printTree left ++ show key  ++ printTree right ++ ")"

