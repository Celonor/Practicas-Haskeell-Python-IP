-- ===============
-- Sopa de números
-- ===============
{- HLINT ignore "Use map" -}
{- HLINT ignore "Use foldr" -}

type Fila = [Int]
type Tablero = [Fila]
type Posicion = (Int,Int)
type Camino = [Posicion]

-- Ejercicio 5

maximoPorFila :: Fila -> Int
maximoPorFila [x] = x
maximoPorFila (x:y:ls) | x >= y = maximoPorFila (x:ls)
                       | otherwise = maximoPorFila (y:ls)

listaMaximos :: Tablero -> [Int]
listaMaximos [] = []
listaMaximos (x:xs) = maximoPorFila x : listaMaximos xs

maximo :: Tablero -> Int
maximo [x] = maximoPorFila x
maximo x = maximoPorFila (listaMaximos x)

-- Ejercicio 6

unirFilas :: Tablero -> Fila
unirFilas [] = []
unirFilas (x:xs) = x ++ unirFilas xs

cantApariciones :: Int -> [Int] -> Int
cantApariciones _ [] = 0
cantApariciones n (x:xs) | n == x = 1 + cantApariciones n xs
                         | otherwise = cantApariciones n xs

sinRepetidos :: [Int] -> [Int]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (limpiar x xs)

competencia :: [Int] -> [Int] -> Int
competencia [x] _ = x
competencia (x:y:candidatos) original 
    | cantApariciones x original >= cantApariciones y original = competencia (x:candidatos) original
    | otherwise = competencia (y:candidatos) original

limpiar :: Int -> Fila -> Fila
limpiar _ [] = []
limpiar n (x:xs) | n == x = limpiar n xs
                 | otherwise = x : limpiar n xs

masRepetido :: Tablero -> Int
masRepetido t = competencia (sinRepetidos (unirFilas t)) (unirFilas t)

-- Ejercicio 7

caminoFila :: Tablero -> Int -> [Int]
caminoFila (l:ls) x | x == 1 = l
                    | otherwise = caminoFila ls (x-1)

caminoColumna :: [Int] -> Int -> Int
caminoColumna (l:ls) y | y == 1 = l
                       | otherwise = caminoColumna ls (y-1)

valorPosicion :: Tablero -> Posicion -> Int
valorPosicion ls (x,y) = caminoColumna (caminoFila ls x) y

valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino _ [] = []
valoresDeCamino l (x:xs) = valorPosicion l x : valoresDeCamino l xs

-- Ejercicio 8

fibonacci :: Int -> Int
fibonacci n = fiboAux n 0 1

fiboAux :: Int -> Int -> Int -> Int
fiboAux 0 a _ = a
fiboAux k a b = fiboAux (k-1) b (a+b)

esCaminoFibo :: [Int] -> Int -> Bool
esCaminoFibo [] _ = True
esCaminoFibo (x:xs) n | x == fibonacci n = esCaminoFibo xs (n+1)
                      | otherwise = False

-- ================
-- Perfectos Amigos
-- ================

-- Ejercicio 9

auxDivisores :: Int -> Int -> [Int]
auxDivisores n k | div n 2 < k = []
                 | mod n k == 0 = k : auxDivisores n (k+1)
                 | otherwise = auxDivisores n (k+1)

divisoresPropios :: Int -> [Int]
divisoresPropios n = auxDivisores n 1

-- Ejercicio 10

sumaDivisores :: [Int] -> Int
sumaDivisores [] = 0
sumaDivisores (x:xs) = x + sumaDivisores xs 

sonAmigos :: Int -> Int -> Bool
sonAmigos n m = sumaDivisores (divisoresPropios n) == m && n == sumaDivisores (divisoresPropios m)

-- Ejercicio 11

nPerfecto :: Int -> Int
nPerfecto n | sumaDivisores (divisoresPropios n) == n = 1
            | otherwise = 0

auxPrimerosNPerfectos :: Int -> Int -> [Int]
auxPrimerosNPerfectos n k | n == 0 = []
                          | nPerfecto k == 1 = k : auxPrimerosNPerfectos (n-1) (k+1)
                          | otherwise = auxPrimerosNPerfectos n (k+1)

losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos n = auxPrimerosNPerfectos n 1

-- Ejerccio 12

tuplaDeAmigos :: Int -> [Int] -> [(Int,Int)]
tuplaDeAmigos _ [] = []
tuplaDeAmigos x (l:ls) | sonAmigos x l = [(x,l)]
                       | otherwise = tuplaDeAmigos x ls

listaDeAmigos :: [Int] -> [(Int,Int)]
listaDeAmigos []  = []
listaDeAmigos (x:xs) = tuplaDeAmigos x xs ++ listaDeAmigos xs
