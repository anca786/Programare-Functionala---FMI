import Data.Maybe
import Data.List (nub)
type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving Eq
infixr 2 :|:
infixr 3 :&:

--1. a) (P∨Q)∧(P∧Q)
pa :: Prop
pa = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

--b) (P∨Q)∧(¬P∧¬Q)
pb :: Prop
pb = (Var "P" :|: Var "Q") :&: (Not(Var "P") :&: Not(Var "Q"))

--c) (P∧(Q∨R))∧((¬P∨¬Q)∧(¬P∨¬R))
pc :: Prop
pc = (Var "P" :&: (Var "Q" :|: Var "R")) :&:((Not(Var "P") :|: Not(Var "Q")) :&: (Not(Var "P") :|: Not(Var "R")))


--2. Faceți tipul Prop instanță a clasei de tipuri Show, înlocuind conectorii Not, :|: și :&: cu ~, | și & și folosind direct numele variabilelor în loc de construcția Var nume.
instance Show Prop where
    show (Var x) = x
    show (Not p) = "(~" ++ show p ++ ")"
    show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"
    show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"
    show (p :->: q) = "(" ++ show p ++ "->" ++ show q ++ ")"
    show (p :<->: q) = "(" ++ show p ++ "<->" ++ show q ++ ")"


type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

--3. Definiți o funcție eval care, dată fiind o expresie logică și un mediu de evaluare, calculează valoarea de adevăr a expresiei.
eval :: Prop -> Env -> Bool
eval (Var x) env = impureLookup x env
eval (Not p) env = not (eval p env)
eval (p :|: q) env = eval p env || eval q env
eval (p :&: q) env = eval p env && eval q env
eval (p :->: q) env = not (eval p env) || eval q env 
eval (p :<->: q) env = eval p env == eval q env
 

--4. Definiți o funcție variabile care colectează lista tuturor variabilelor dintr-o formulă. Hint: folosiți funcția nub.
variabile :: Prop -> [Nume]
variabile (Var x) = [x]
variabile (Not p) = variabile p
variabile (p :|: q) = nub (variabile p ++ variabile q)
variabile (p :&: q) = nub (variabile p ++ variabile q)
variabile (p :->: q) = nub (variabile p ++ variabile q)
variabile (p :<->: q) = nub (variabile p ++ variabile q)

test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]


--5. Dată fiind o listă de nume, definiți toate atribuirile de valori de adevăr posibile pentru ea.
envs :: [Nume] -> [Env]
envs [] = [[]]
envs (x:xs) = [(x,b) : l | b <- [True,False], l <- envs xs]


--6. Definiți o funcție satisfiabila care, dată fiind o propoziție, verifică dacă aceasta este satisfiabilă. Hint: puteți folosi rezultatele de la exercițiile 4 și 5.
satisfiabila :: Prop -> Bool
satisfiabila p = or [eval p l | l <- envs(variabile p)]


--7. O propoziție este validă dacă se evaluează la True pentru orice interpretare a variabilelor. O formulare echivalentă este aceea că o propoziție este validă dacă negația ei este nesatisfiabilă. Definiți o funcție valida care verifică dacă o propoziție este validă.
valida :: Prop -> Bool
valida p = not (satisfiabila (Not p))


--8. Extindeți tipul de date Prop și funcțiile definite până acum pentru a include conectorii logici -> (implicație) și <-> (echivalență), folosind constructorii :->: și :<->:.

--9. Două propoziții sunt echivalente dacă au mereu aceeași valoare de adevăr, indiferent de valorile variabilelor propoziționale. Scrieți o funcție care verifică dacă două propoziții sunt echivalente.
echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2 = valida (p1 :<->: p2)

test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))



