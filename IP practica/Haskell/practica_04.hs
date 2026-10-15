import GHC.Exts.Heap (GenClosure(key))
-- Ejercicio 1

fibonacci :: Integer -> Integer
fibonacci x | x == 0 = 0
            | x == 1 = 1
            | otherwise = (fibonacci (x-1)) + (fibonacci (x-2))

-- Ejercicio 2

parteEntera :: Float -> Integer
parteEntera x | x < 1 = 0
              | otherwise = 1 + parteEntera (x-1)

-- Ejercicio 3

restando :: Integer -> Integer -> Integer
restando x y | x < y = x
               | otherwise = restando (x-y) y

esDivisible :: Integer -> Integer -> Bool
esDivisible x y | restando x y == 0 = True
                | otherwise = False

-- Ejercicio 4

--Aca literalmente con n*n se arregla

sumaImpares :: Integer -> Integer
sumaImpares x | x == 0 = 0
              | otherwise = (x*2 - 1) + sumaImpares (x-1)

-- Ejercicio 5 

medioFact :: Integer -> Integer
medioFact x | x <= 1 = 1
            | x == 2 = 2
            | otherwise = x * medioFact (x-2)

-- Ejercicio 6 

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x | x < 10 = True
                      | mod (div x 10) 10 /= mod x 10 = False
                      | otherwise = todosDigitosIguales (div x 10)

-- Ejercicio 7

potencia :: Integer -> Integer -> Integer
potencia x y | y == 0 = 1
             | otherwise = x * potencia x (y-1)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (potencia 10 (cantDigitos n - i))) 10

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (div n 10)

-- Ejercicio 8

sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 10 = n
              | otherwise = mod n 10 + sumaDigitos (div n 10)

--Ejercicio 9 

esCapicua :: Integer -> Bool
esCapicua n | n < 10 = True
            | otherwise = iesimoDigito n 1 == iesimoDigito n (cantDigitos n) && esCapicua (div (mod n (potencia 10 (cantDigitos n - 1))) 10 )

-- Ejercicio 10

f1 :: Integer -> Integer
f1 n | n == 0 = 1
     | otherwise = potencia 2 n + f1 (n-1)

f2 :: Integer -> Float -> Float
f2 n q | n == 1 = q
       | otherwise = q^n + f2 (n-1) q

f3 :: Integer -> Float -> Float
f3 n q | n == 0 = 0
       | otherwise = f2 (2*n) q

f4 :: Integer -> Float -> Float
f4 n q | n == 0 = 1
       | otherwise = f2 (2*n) q - f2 (n-1) q

-- Ejercicio 11

factorial :: Float -> Float
factorial x | x == 0 = 1.0
            | otherwise = x * factorial (x-1) 

aFloat :: Integer -> Float
aFloat n | n == 0 = 0
         | otherwise = 1.0 + aFloat (n-1)

eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | otherwise = (1 / factorial (aFloat n)) + eAprox (n-1)

e :: Float
e = eAprox 9

-- Ejercicio 12

a :: Integer -> Float
a n | n == 1 = 2.0
    | otherwise = 2.0 + (1.0 / a (n-1))

raizDeAprox :: Integer -> Float
raizDeAprox n = a n - 1.0

-- Ejercicio 13

eme :: Integer -> Integer -> Integer
eme n m | m == 1 = n
    | otherwise = n^m + eme n (m-1)

efe :: Integer -> Integer -> Integer
efe n m | n == 1 = eme n m
        | otherwise = eme n m + efe (n-1) m

-- Ejercicio 14

ayudaPot :: Integer -> Integer -> Integer -> Integer
ayudaPot q m n | m == 1 = q^(n+1)
               | otherwise = q^(n+m) + ayudaPot q (m-1) n

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q m n | n == 1 = ayudaPot q m n
                    | otherwise = ayudaPot q m n + sumaPotencias q m (n-1)

-- Ejercicio 15

ayudaRac :: Integer ->Integer ->Float
ayudaRac n m | m == 1 = fromIntegral n
             | otherwise = fromIntegral n/ fromIntegral m + ayudaRac n (m-1)

sumaRacionales :: Integer ->Integer ->Float
sumaRacionales n m | n == 1 = ayudaRac n m
                   | otherwise = ayudaRac n m + sumaRacionales (n-1) m

-- Ejercicio 16

-- a) 

auxDiv :: Integer -> Integer -> Integer
auxDiv n k | mod n k == 0 = k
           | otherwise = auxDiv n (k+1)

menorDivisor :: Integer -> Integer
menorDivisor n = auxDiv n 2

-- b)

esPrimo :: Integer -> Bool
esPrimo n = auxDiv n 2 == n
          
-- c)

auxPrimos :: Integer -> Integer -> Integer -> Bool
auxPrimos n m k | (mod n k == 0)  &&  (mod m k == 0) = False
                | (n == k && mod m k /= 0) || (m == k && mod n k /= 0) = True
                | otherwise = auxPrimos n m (k+1)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m | n == 1 || m == 1 = True
                | otherwise = auxPrimos n m 2

-- d)

auxM :: Integer -> Integer -> Integer
auxM n k | mod n k == 0 = k
         | otherwise = auxM n (k+1)

mPrimo :: Integer -> Integer
mPrimo m | auxM m 2 == m = m
         | otherwise = mPrimo (m+1)

auxEnes :: Integer -> Integer -> Integer -> Integer 
auxEnes n k m | k == n = m
              | otherwise = auxEnes n (k+1) (mPrimo (m+1))

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = auxEnes n 1 2

-- Ejercicio 17

auxEsFibo :: Integer -> Integer -> Bool
auxEsFibo n k | n == fibonacci k = True
              | n < fibonacci k = False
              | otherwise = auxEsFibo n (k+1)

esFibonacci :: Integer -> Bool
esFibonacci n = auxEsFibo n 0

-- Ejercicio 18

mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n | n < 10 && mod n 2 == 0 = n
                 | n < 10 = -1
                 | mod (mod n 10) 2 /= 0 = mayorDigitoPar (div n 10)
                 | mod n 10 >= mayorDigitoPar (div n 10) = mod n 10
                 | otherwise = mayorDigitoPar (div n 10)

-- Ejercicio 19

sumPrimos :: Integer -> Integer
sumPrimos 0 = 0
sumPrimos 1 = nEsimoPrimo 1
sumPrimos k = nEsimoPrimo k + sumPrimos (k-1)

auxSumPrimos :: Integer -> Integer -> Bool
auxSumPrimos n k | n == sumPrimos k = True
                 | n < sumPrimos k = False
                 | otherwise = auxSumPrimos n (k+1)

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = auxSumPrimos n 0

-- Ejercicio 20

auxSumaDivisores :: Integer -> Integer -> Integer
auxSumaDivisores n k | k == n = n
                     | mod n k == 0 = k + auxSumaDivisores n (k+1)
                     | otherwise = 0 + auxSumaDivisores n (k+1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = auxSumaDivisores n 1

tomaValorMax :: Integer -> Integer -> Integer
tomaValorMax n1 n2 | n1 == n2 = n1
                   | sumaDivisores n1 >= tomaValorMax (n1+1) n2 = n1
                   | otherwise = tomaValorMax (n1+1) n2 