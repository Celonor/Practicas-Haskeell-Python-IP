import System.Console.Haskeline (Interrupt)
estanRelacionados :: Integer-> Integer-> Bool
estanRelacionados x y = mod x y == 0

type Punto2D = (Float, Float)

productoInterno :: Punto2D -> Punto2D -> Float
productoInterno (a,b) (c,d) = a*c + b*d

esParMenor :: Punto2D -> Punto2D -> Bool
esParMenor (a,b) (c,d) = (a<c) && (b<d)

distancia :: Punto2D -> Punto2D -> Float
distancia (a,b) (c,d) = sqrt (((c-a)**2) + ((d-b)**2))

sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (x,y,z) = x+y+z

dandoValor :: Integer -> Integer -> Integer
dandoValor x y | mod x y == 0 = x
               | otherwise = 0

sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (x,y,z) a = (dandoValor x a) + (dandoValor y a) + (dandoValor z a)

posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (x,y,z) | mod x 2 == 0 = x
                     | mod y 2 == 0 = y
                     | mod z 2 == 0 = z
                     | otherwise = 4

crearPar :: a -> b -> (a,b)
crearPar a b = (a,b)

invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)

-- Ejercicio 5

todosMenores :: (Integer, Integer, Integer)-> Bool
todosMenores (x,y,z)= ((f x)>(g x)) && ((f y)>(g y)) && ((f z)>(g z))

f :: Integer -> Integer
f x | x <= 7 = x*x
    | otherwise = 2*x-1

g :: Integer -> Integer
g x | mod x 2 == 0 = div x 2
    | otherwise = 3*x+1

--Ejercicio 6

type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto x | mod x 4 /= 0 = False
           | (mod x 400 /= 0) && (mod x 100 == 0) = False
           | otherwise = True