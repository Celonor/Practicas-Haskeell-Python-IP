doubleMe :: Integer -> Integer
doubleMe x = x + x

f :: Integer -> Integer
f x | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16

g :: Integer -> Integer
g x | x == 8 = 16
    | x == 16 = 4
    | x == 131 = 1

h :: Integer -> Integer
h x = f (g x)

k :: Integer -> Integer
k x = g (f x)

absoluto :: Integer -> Integer
absoluto x | x >= 0 = x
    | x < 0 = -x

maximoAbsoluto :: Integer -> Integer-> Integer
maximoAbsoluto x y | (absoluto x) > (absoluto y) = (absoluto x)
maximoAbsoluto x y = (absoluto y)

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | (maximoAbsoluto x y) > (absoluto z) = (maximoAbsoluto x y)
maximo3 x y z = (absoluto z)

algunoEsCero :: Integer -> Integer -> Bool
algunoEsCero x y = x == 0 || y == 0

ambosSonCero :: Integer -> Integer -> Bool
ambosSonCero x y = x == 0 && y == 0

cualIntervalo :: Float -> Float
cualIntervalo x | x <= 3 = 1
    | x > 7 = 2
    | otherwise = 3

enMismoIntervalo :: Float -> Float -> Bool
enMismoIntervalo x y = (cualIntervalo x) == (cualIntervalo y)

repetidos:: Integer -> Integer -> Integer
repetidos x y | x == y = x
    | otherwise = x + y

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x == y && x == z = x
    | x == y = (repetidos y z)
    | x == z = (repetidos z y)
    | y == z = (repetidos z x)
    | otherwise = x+y+z

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod x y == 0

digitoUnidades :: Integer -> Integer
digitoUnidades x = (div x 10)

digitoDecenas :: Integer -> Integer
digitoDecenas x = ((div (div x 10) 10) * 10) + (mod x 10)