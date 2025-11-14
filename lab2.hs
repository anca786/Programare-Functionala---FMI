myInt=5163165261635415

double :: Integer -> Integer
double x = x+x

maxim :: Integer -> Integer -> Integer
maxim x y =
    if (x > y)
        then x
        else y

maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z =
    if (x > y && x > z)
        then x
        else if (y > z)
            then y
            else z


maxim4 :: Integer -> Integer -> Integer -> Integer -> Integer
maxim4 a b c d=
    let 
        u=maxim3 a b c
    in 
        maxim u d


suma :: Integer -> Integer -> Integer
suma x y = x*x + y*y

paritate :: Integer -> Bool
paritate x =
    if (mod x 2 ==0)
        then True
        else False

factorial :: Integer -> Integer
factorial n
    | n==0 = 1
    |otherwise = n * factorial (n-1)

verif :: Integer -> Integer -> Integer
verif x y =
    if(x > y + y)
        then 1
        else 0

maximLista :: [Integer] -> Integer
maximLista [] = 0
maximLista [x] = x
maximLista (x:xs) =
    let
        m = maximLista xs
    in 
        maxim x m


poly :: Integer -> Integer -> Integer -> Integer -> Integer
poly a b c x = a*x^2 + b*x + c



eeny :: Integer -> String 
eeny n =
    if (even n)
        then "eeny"
        else "meeny"


fizzbuzz :: Integer -> String
fizzbuzz n
    |mod n 3 ==0 && mod n 5 ==0 = "FizzBuzz"
    |mod n 3 == 0 = "Fizz"
    |mod n 5 == 0 = "Buzz"
    |otherwise = ""

fizzbuzz2 :: Integer -> String
fizzbuzz2 n =
    if(mod n 3==0 && mod n 5 == 0)
        then "FizzBuzz"
        else if(mod n 3==0)
            then "Fizz"
            else if(mod n 5==0)
                then "Buzz"
                else ""


tribonacci :: Integer -> Integer
tribonacci n
    |n<3 =1
    |n==3 =2
    |otherwise = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3)


tribonacciEcuational :: Integer -> Integer
tribonacciEcuational 1 = 1
tribonacciEcuational 2 = 1
tribonacciEcuational 3 = 2
tribonacciEcuational n =
    tribonacciEcuational(n-1) + tribonacciEcuational(n-2) + tribonacciEcuational(n-3)



binomial :: Integer -> Integer -> Integer
binomial n k
    |k==0 = 1
    |n==0 = 0
    |otherwise = binomial(n-1) k + binomial(n-1)(k-1)