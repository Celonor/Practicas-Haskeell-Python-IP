-- Ejercicio 1

sumDivisores :: Integer -> Integer -> Integer
sumDivisores x k | k > div x 2 = 0
sumDivisores x k | mod x k == 0 = k + sumDivisores x (k+1)
                 | otherwise = sumDivisores x (k+1) 

abundante :: Integer -> Integer
abundante n | sumDivisores n 1 > n = 1
            | otherwise = 0

f1 :: Integer -> Integer -> Integer
f1 d h | d == h = abundante d
       | otherwise = abundante d + f1 (d+1) h

-- Ejercicio 2

abajoDel2022 ::  [(String,Integer,Integer)] -> [(String,Integer,Integer)]
abajoDel2022 [] = []
abajoDel2022 ((x,y,z):ls) | y < 2022 = (x,y,z) : abajoDel2022 ls
                          | otherwise = abajoDel2022 ls

quitarSegundoCuatri :: [(String,Integer,Integer)] -> [String]
quitarSegundoCuatri [] = []
quitarSegundoCuatri ((x,y,z):ls) | y == 2021 && z == 2 = quitarSegundoCuatri ls
                                 | otherwise = x : quitarSegundoCuatri ls

quitarUno :: String -> [String] -> [String]
quitarUno x [] = []
quitarUno x (y:ys) | x == y = quitarUno x ys
                     | otherwise = y : quitarUno x ys

quitarTodos :: [String] -> [String]
quitarTodos [] = []
quitarTodos (x:xs) = x : quitarTodos (quitarUno x xs)

f2 :: [(String,Integer,Integer)] -> [String]
f2 x = quitarTodos (quitarSegundoCuatri (abajoDel2022 x))

-- EjerciAcio 3

f3 :: [Integer] -> Integer -> [Integer]
f3 [] _ = []
f3 (x:xs) n | x < 0 = []
            | x >= n = n : f3 xs n
            | otherwise = x : f3 xs n

-- Ejercicio 4

posicionPar :: [Integer] -> Integer -> Integer
posicionPar _ 0 = 0
posicionPar (x:xs) n | (n == 1) && (mod x 2 == 0) = 1
                     | otherwise = posicionPar xs (n-1)

f4 :: [[Integer]] -> Integer -> Integer
f4 [] _ = 0
f4 (x:xs) n = (posicionPar x n) + (f4 xs n)